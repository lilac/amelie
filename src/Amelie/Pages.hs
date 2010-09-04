{-# LANGUAGE NamedFieldPuns, FlexibleContexts, OverloadedStrings #-}
module Amelie.Pages where

import           Control.Applicative         ((<$>))
import           Control.Applicative.Error   (Failing(..))
import           Control.Arrow               ((***))
import           Control.Monad.State         (MonadState)
import           Control.Monad.Trans         (MonadIO)
import           Data.List                   (find)
import           Data.Maybe                  (isJust)
import           Data.Monoid                 (mconcat,mempty)

import           Codec.Binary.UTF8.String    (decodeString,encodeString)
import           Data.Text                   (pack)
import           Data.Time.Instances         ()
import           Network.CGI                 (CGIResult)
import qualified Network.CGI                 as CGI
import           Network.CGI.Monad           (MonadCGI(..))
import           Safe                        (readMay)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Utf8    (renderHtml)

import           Amelie.DB                   (db)
import qualified Amelie.DB                   as DB
import           Amelie.HTML                 (pasteForm,pastesHtmlTable,pasteInfoHtml,
                                              pastePasteHtml)
import           Amelie.Links                (link)
import           Amelie.Pages.Error          (errorPage)
import           Amelie.Templates            (template)
import           Amelie.Types                (State(..),Paste(..),ChansAndLangs,SCGI,
                                              Language(..))
import           Amelie.Utils                (text,l2s)

-- | A CGI page of all pastes.
pastesPage :: (MonadState State m,MonadCGI m,MonadIO m,Functor m)
              => m CGIResult
pastesPage = do
  pastes <- db $ DB.allPastesLimitBy 20
  cl <- db DB.chansAndLangs
  let latestPastes = l2s $ renderHtml $ pastesHtmlTable pastes
      newPasteForm = l2s $ renderHtml $
                     newPasteHtml Nothing $ snd $ pasteForm cl []
  template "All Pastes" "pastes" 
           [("latest_pastes",latestPastes)
           ,("new_paste_form",newPasteForm)]
           Nothing

-- | Render a pretty highlighted paste with info.
pastePage :: [(String, String)] -> ChansAndLangs -> SCGI CGIResult
pastePage = asPastePage $ \ps cl@(_,langs) paste@Paste{title} ->
  let lang = lookup "lang" ps >>= \name -> find ((==name) . langName) langs
      info = l2s $ renderHtml $ pasteInfoHtml lang cl paste
      paste' = l2s $ renderHtml $ pastePasteHtml paste lang
  in template title "paste" [("info",info),("paste",paste')] Nothing

-- | Render a raw paste.
rawPastePage :: [(String, String)] -> ChansAndLangs -> SCGI CGIResult
rawPastePage = asPastePage $ \_ _ Paste{content} -> do
  CGI.setHeader "Content-Type" "text/plain; charset=UTF-8"
  CGI.output $ encodeString content

-- | A CGI page to show a paste.
asPastePage :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => ([(String,String)] -> ChansAndLangs -> Paste -> m CGIResult)
             -> [(String,String)]             
             -> ChansAndLangs
             -> m CGIResult
asPastePage run ps cl =
    case lookup "pid" ps >>= readMay of
      Nothing    -> noPasteGiven
      Just pid'' -> do
        result <- db $ DB.pasteById pid'' cl
        case result of
          Nothing    -> noSuchPaste
          Just paste -> run ps cl paste
  where noSuchPaste = errorPage "Unknown paste."
        noPasteGiven = errorPage "No paste given."

-- | A CGI page to create a new paste.
newPastePage :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => ChansAndLangs -> m CGIResult
newPastePage cl = do
  inputs <- map (decodeString *** decodeString) <$> CGI.getInputs
  submitted <- isJust <$> CGI.getInput "submit"
  let (result,formHtml) = pasteForm cl inputs
  case result of
    Failure errs -> pageWithErrors errs formHtml submitted
    Success paste -> do pid' <- db $ DB.createPaste paste
                        CGI.redirect $ link "paste" [("pid",show pid')
                                                    ,("title",title paste)]
  where pageWithErrors errs form submitted =
          template "New paste" "new" [] $ Just $
            newPasteHtml (Just (submitted,errs)) form

-- | The HTML container/submitter/error displayer for the paste form.
newPasteHtml :: Maybe (Bool,[String]) -> String -> H.Html
newPasteHtml s form = do
  case s of 
    Just (True,errs@(_:_)) -> do
      H.p ! A.class_ "errors" $ text "There were some problems with your input:"
      H.ul . mconcat . map (H.li . H.text . pack) $ errs
    _ -> mempty
  H.form ! A.method "post" ! A.action "/new" $ do
    H.preEscapedString form
    H.input ! A.type_ "submit" ! A.value "Create Paste" ! A.class_ "submit"
    H.input ! A.type_ "hidden" ! A.value "true" ! A.name "submit"
