{-# LANGUAGE NamedFieldPuns, FlexibleContexts, OverloadedStrings #-}
module Amelie.Pages where

import           Control.Applicative        ((<$>))
import           Control.Applicative.Error  (Failing(..))
import           Control.Arrow              ((***))
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans        (MonadIO)
import           Data.List                  (find)
import           Data.Maybe                 (isJust,fromMaybe)

import           Codec.Binary.UTF8.String   (decodeString,encodeString)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L (concat)
import           Data.Time.Instances        ()
import           Network.CGI                (CGIResult)
import qualified Network.CGI                as CGI
import           Network.CGI.Monad          (MonadCGI(..))
import           Safe                       (readMay)
import           Text.Blaze.Renderer.Utf8   (renderHtml)

import           Amelie.DB                  (db)
import qualified Amelie.DB                  as DB
import           Amelie.HTML                 (pasteForm,pastesHtmlTable
                                             ,pasteInfoHtml,pastePasteHtml
                                             ,controlPasteHtml)
import           Amelie.Links               (link)
import           Amelie.Pages.Error         (errorPage)
import           Amelie.Templates           (template,renderTemplate)
import           Amelie.Types                (State(..),Paste(..),
                                              ChansAndLangs,SCGI,
                                              Language(..))
import           Amelie.Utils               (l2s,text)

-- | A CGI page of all pastes.
pastesPage :: (MonadState State m,MonadCGI m,MonadIO m,Functor m)
              => m CGIResult
pastesPage = do
  pastes <- db $ DB.allPastesLimitBy 20
  cl <- db DB.chansAndLangs
  let latestPastes = l2s $ renderHtml $ pastesHtmlTable pastes
      newPasteForm = l2s $ renderHtml $
                     controlPasteHtml "Create paste"
                        Nothing (snd $ pasteForm Nothing cl []) Nothing
  template "All Pastes" "pastes" 
           [("latest_pastes",latestPastes)
           ,("new_paste_form",newPasteForm)]
           Nothing

-- | Render a pretty highlighted paste with info.
pastePage :: [(String, String)] -> ChansAndLangs -> SCGI CGIResult
pastePage = asPastePage page where
  page ps cl main@Paste{pid=main_id,title=mainTitle,annotation_of} = do
      pastes <- db $ DB.pastesByAnnotationOf main_id cl
      annotated <- case annotation_of of
        Just anid -> db $ DB.pasteById anid cl
        Nothing   -> return Nothing      
      let paste_renders = renderPaste ps cl annotated main
                          : map (renderPaste ps cl (Just main)) pastes
      rendered <- sequence <$> sequence paste_renders
      annotateForm <- annotate cl main
      case rendered of
        Right (html:htmls) -> do
          let htmls' 
                | null htmls = []
                | otherwise = "<h2 class='annotations'>Annotations</h2>" : htmls
          template mainTitle "paste" [("paste",l2s html)
                                     ,("annotations",l2s $ L.concat htmls')
                                     ,("annotate",l2s annotateForm)]
                   Nothing
        Right _     -> errorPage "No paste."
        Left err    -> errorPage err

annotate :: ChansAndLangs -> Paste -> SCGI L.ByteString
annotate cl Paste{pid=annotation_of} = do
  inputs <- map (decodeString *** decodeString) <$> CGI.getInputs
  let (_,formHtml) = pasteForm Nothing cl inputs
  return $ renderHtml $ controlPasteHtml "Annotate paste"
             Nothing formHtml (Just annotation_of)

-- | Try to a paste info and content to HTML string.
renderPaste :: (MonadIO m, MonadState State m)
               => [(String, String)] -> ChansAndLangs
               -> Maybe Paste
               -> Paste
               -> m (Either String L.ByteString)
renderPaste ps cl@(_,langs) annotation_of paste@Paste{pid,title} = 
    renderTemplate "info_paste" params where
  params = [("title",l2s . renderHtml . text $ title)
           ,("info",info)
           ,("paste",paste')]
  info = l2s $ renderHtml $ pasteInfoHtml lang cl paste annotation_of
  paste' = l2s $ renderHtml $ pastePasteHtml paste lang
  lang = lookup lparam ps >>= \name -> find ((==name) . langName) langs
  lparam = "lang_" ++ show pid

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
editCreatePastePage :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => [(String,String)] -> ChansAndLangs -> m CGIResult
editCreatePastePage params cl = do
  inputs <- map (decodeString *** decodeString) <$> CGI.getInputs
  submitted <- isJust <$> CGI.getInput "submit"
  annotation_of <- (>>=readMay) <$> CGI.getInput "annotation_of"
  let edit = lookup "pid" params >>= readMay
  epaste <- case edit of
    Just eid -> db $ DB.pasteById eid cl
    Nothing  -> return Nothing
  let (result,formHtml) = pasteForm epaste cl inputs
  case result of
    Failure errs -> let title | isJust epaste = "Edit paste" 
                              | otherwise     = "New paste"
                    in pageWithErrors title errs formHtml submitted
    Success paste -> do pid' <- insertOrUpdate paste annotation_of
                        let pasteid = fromMaybe pid' annotation_of
                        CGI.redirect $ link "paste" $ 
                          [("pid",show pasteid)
                          ,("title",title paste)]
                          ++ [("annotation",show pid') | isJust annotation_of]
  where pageWithErrors title errs form submitted =
          template title "control" [] $ Just $
            controlPasteHtml title (Just (submitted,errs)) form Nothing
        insertOrUpdate paste@Paste{pid} an_of =
          if pid > 0
             then do db $ DB.updatePaste paste; return pid
             else db $ DB.createPaste paste an_of