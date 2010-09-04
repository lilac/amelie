{-# LANGUAGE DeriveDataTypeable, RecordWildCards, RankNTypes, NamedFieldPuns, 
             DisambiguateRecordFields, TupleSections, GeneralizedNewtypeDeriving, 
             ScopedTypeVariables, FlexibleContexts, FlexibleInstances, 
             OverloadedStrings #-}
module Amelie.HTML where

import           Control.Applicative         (Applicative)
import           Control.Applicative         (pure,(<$>),(<*>),(*>))
import           Control.Applicative.Error   (Failing(..))
import           Control.Arrow               (second)
import           Control.Monad               (ap)
import           Control.Monad               (mplus)
import           Control.Monad.Identity      (Identity)
import           Control.Monad.Identity      (runIdentity)
import           Data.List                   (find)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (mconcat,mempty)
import           Data.Time                   (UTCTime,formatTime)
import           Safe                        (readMay)
import           System.Locale               (defaultTimeLocale)

import qualified Data.List.Utils             as List (replace)
import           Data.Text                   (pack)
import           Data.Time.Instances         ()
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Formlets               as X
import           Text.XHtml.Strict           ((<<))
import qualified Text.XHtml.Strict           as Html
import qualified Text.XHtml.Strict.Formlets  as XH

import           Amelie.Highlight            (highlightCSS,pasteHighlightedHtml)
import           Amelie.Links                (link)
import           Amelie.Types                (Language(..),Channel(..),Paste(..),
                                              ChansAndLangs)
import           Amelie.Utils                (text)

-- | HTML of pastes list.
pastesListHtml :: [Paste] -> H.Html
pastesListHtml = H.ul . mconcat . map pasteLi where
  pasteLi Paste{..} = H.li $ do H.a ! A.href (H.stringValue url) $ text title
                                H.text " by "
                                text author
    where url = link "paste" [("pid",show pid),("title",title)]

-- | HTML table representation of a pastes list.
pastesHtmlTable :: [Paste] -> H.Html
pastesHtmlTable = table . H.tbody . mconcat . map pasteRowHtml where
  table tbody = H.table $ do thead; tbody
  thead = H.thead $ H.tr $ mconcat $ map (H.th . text) fields
  fields = ["Title","Author","When","Language","Channel"]
  pasteRowHtml Paste{..} = H.tr $ do
    H.td $ H.a ! A.href (H.stringValue url) $ text title
    td author
    maybe mempty ((H.td ! A.class_ "utctime") . text . formatUTC) created
    td $ maybe "-" langTitle language
    td $ maybe "-" chanName channel
    where td = H.td . H.text . pack
          url = link "paste" [("pid",show pid),("title",title)]

-- | Paste info of a paste.
pasteInfoHtml :: Maybe Language -> ChansAndLangs -> Paste -> Maybe Paste 
                 -> H.Html
pasteInfoHtml lang cl paste@Paste{..} an_of = do
  H.a ! attr A.id anchor ! attr A.name anchor $ mempty
  H.ul $ do def "Paste" $ href (self "paste") $ text $ '#' : show pid
            maybe mempty (def "Annotation of" . annotation) an_of
            def "Author" $ text author
            maybe mempty (def "Channel" . text . chanName) channel
            def "Created" $ H.span ! aid "created" $ text creationDate
            def "Raw" $ href (self "raw") $ text "View raw file"
            def "Language" $ displayLangSwitcher lang cl paste
  where def t dd = H.li $ do H.strong $ text $ t ++ ":"; H.span dd
        attr f a = f (H.stringValue a)
        aid = A.id -- To appease hlint, for now.
        self typ = link typ [("pid",show pid),("title",title)]
        href l c = H.a ! A.href (H.stringValue l) $ c
        creationDate = fromMaybe "" $ fmap formatUTC created
        anchor = "p" ++ show pid
        annotation Paste{pid=pid',title=title'} =
          href url $ text $ "#" ++ show pid'
          where url = link "paste" [("pid",show pid'),("title",title')]
          

formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

-- | Change the display language of this paste.
displayLangSwitcher :: Maybe Language -> ChansAndLangs -> Paste -> H.Html
displayLangSwitcher paramLang (_,langs) Paste{title,pid,language} =
  H.form ! A.class_ "lang-switcher" ! A.action (H.stringValue self) $ do
    H.input ! A.type_ "hidden" ! A.name "pid" ! attr A.value (show pid)
    H.select ! attr A.name lparam $ mconcat $ empty : map showLang langs
    H.input ! A.type_ "submit" ! A.value "Change display"
  where self = link "paste" [("pid",show pid),("title",title)]
        attr f a = f (H.stringValue a)
        empty = H.option ! A.value "" $ text ""
        showLang lang'@Language{..} =
          let opt = H.option ! attr A.value langName $ text langTitle
          in if lang == Just lang' then opt ! A.selected "selected" else opt
        lang = paramLang `mplus` language
        lparam = "lang_" ++ show pid

-- | Paste HTML of a paste.
pastePasteHtml :: Paste -> Maybe Language -> H.Html
pastePasteHtml paste@Paste{..} lang = do
  H.style $ text highlightCSS
  H.div $ H.preEscapedString $ fromMaybe (plain $ Html.thecode << content) $ 
    pasteHighlightedHtml paste lang
    where plain = List.replace "\n" "<br>" . Html.showHtmlFragment

-- | An identity monad for running forms, with Applicative instance.
newtype RunForm a = RF { runForm :: Identity a } deriving (Monad,Functor)
instance Applicative RunForm where (<*>) = ap; pure = return

-- | The HTML container/submitter/error displayer for the paste form.
controlPasteHtml :: String -> Maybe (Bool,[String]) -> String -> Maybe Int
                -> H.Html
controlPasteHtml title s form annotation_of = do
  case s of 
    Just (True,errs@(_:_)) -> do
      H.p ! A.class_ "errors" $ text "There were some problems with your input:"
      H.ul . mconcat . map (H.li . H.text . pack) $ errs
    _ -> mempty
  H.form ! A.method "post" ! A.action "/control" $ do
    H.preEscapedString form
    H.input ! A.type_ "submit" ! A.value (H.stringValue title) ! A.class_ "submit"
    H.input ! A.type_ "hidden" ! A.value "true" ! A.name "submit"
    maybe mempty annotate annotation_of
  where annotate p =
          H.input ! A.type_ "hidden" ! A.value (H.stringValue $ show p) 
                  ! A.name "annotation_of"

-- | A form for controlling pastes; creating, editing.
pasteForm :: Maybe Paste -> ChansAndLangs -> [(String,String)]
             -> (Failing Paste,String)
pasteForm paste (chans,langs) inputs =
  runIdentity $ runForm resultAndHtml where
  resultAndHtml = (,Html.renderHtmlFragment html) <$> run
  (run,html,_) = X.runFormState env form
  env = map (second Left) inputs
  form = X.plug Html.ulist $ 
     Paste <$> (fromMaybe 0 . (>>=readMay) <$> editId paste)
           <*> label "Title"    nempty (XH.input $ title <$> paste)
           <*> label "Author"   nempty (XH.input $ author <$> paste)
           <*> label "Language" we languageInput
           <*> label "Channel"  we channelInput
           <*> label "Paste"    nempty (clean <$> pasteInput (content <$> paste))
           <*> pure []
           <*> pure Nothing
           <*> pure Nothing
  languageInput = select (paste >>= language) lid makeLangChoice langs
  channelInput = select (paste >>= channel) cid makeChanChoice chans
  select cur acc make xs =
    look acc xs <$> XH.select [] (empty ++ map make xs) (acc <$> cur)
      where  empty = [(0,"")]
  look acc xs x = find ((==x).acc) xs
  makeLangChoice Language{lid,langTitle} = (lid,langTitle)
  makeChanChoice Channel{cid,chanName} = (cid,chanName)
  pasteInput c = X.plug Html.thediv $ XH.textarea (Just 10) (Just 50) c
  clean = filter (/='\r')
  nempty = not . null
  we = const True

-- | Embed the id of the paste we're editing in the form.
editId :: Maybe Paste -> XH.Form Html.Html RunForm (Maybe String)
editId pid = XH.optionalInput $ \name ->
  case pid of
    Nothing   -> Html.noHtml
    Just Paste{pid=pid'} ->
      Html.input Html.! [Html.name name,Html.thetype "hidden"
                        ,Html.value $ show pid']

-- | Label an input and apply a predicate to it for making inputs required.
label :: (Show a,Monad m,Applicative m) =>
          String -> (a -> Bool) -> X.Form Html.Html m a -> X.Form Html.Html m a
label caption p inp = li $ label' *> (inp `X.check` X.ensure p msg) where
  label' = XH.label (caption ++ ": ")
  msg = caption ++ ": must be provided"
  li = X.plug Html.li