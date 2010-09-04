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
pasteInfoHtml :: Maybe Language -> ChansAndLangs -> Paste -> H.Html
pasteInfoHtml lang cl paste@Paste{..} = 
  H.ul $ do def "Paste" $ href (self "paste") $ text $ '#' : show pid
            def "Author" $ text author
            maybe mempty (def "Channel" . text . chanName) channel
            def "Created" $ H.span ! aid "created" $ text creationDate
            def "Raw" $ href (self "raw") $ text "View raw file"
            def "Language" $ displayLangSwitcher lang cl paste
  where def t dd = H.li $ do H.strong $ text $ t ++ ":"; H.span dd
        aid = A.id -- To appease hlint, for now.
        self typ = link typ [("pid",show pid),("title",title)]
        href l c = H.a ! A.href (H.stringValue l) $ c
        creationDate = fromMaybe "" $ fmap formatUTC created

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

-- | A form for submitting a new paste.
pasteForm :: ChansAndLangs -> [(String,String)] -> (Failing Paste,String)
pasteForm (chans,langs) inputs = runIdentity $ runForm resultAndHtml where
  resultAndHtml = (,Html.renderHtmlFragment html) <$> run
  (run,html,_) = X.runFormState env form
  env = map (second Left) inputs
  form = X.plug Html.ulist $ 
           Paste <$> pure 0
                 <*> label "Title"    nempty (XH.input Nothing)
                 <*> label "Author"   nempty (XH.input Nothing)
                 <*> label "Language" we (select lid makeLangChoice langs)
                 <*> label "Channel"  we (select cid makeChanChoice chans)
                 <*> label "Paste"    nempty (clean <$> pasteInput)
                 <*> pure []
                 <*> pure Nothing
  select acc make xs = look acc xs <$> XH.select [] (empty ++ map make xs) Nothing
  look acc xs x = find ((==x).acc) xs
  makeLangChoice Language{lid,langTitle} = (lid,langTitle)
  makeChanChoice Channel{cid,chanName} = (cid,chanName)
  empty = [(0,"")]
  pasteInput = X.plug Html.thediv $ XH.textarea (Just 10) (Just 50) Nothing
  clean = filter (/='\r') -- For some reason highlighting-kate counts \r\n as 2 lines.
  nempty = not . null
  we = const True

-- | Label an input and apply a predicate to it for making inputs required.
label :: (Show a,Monad m,Applicative m) =>
          String -> (a -> Bool) -> X.Form Html.Html m a -> X.Form Html.Html m a
label caption p inp = li $ label' *> (inp `X.check` X.ensure p msg) where
  label' = XH.label (caption ++ ": ")
  msg = caption ++ ": must be provided"
  li = X.plug Html.li
