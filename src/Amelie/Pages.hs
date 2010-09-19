{-# LANGUAGE NamedFieldPuns, FlexibleContexts, OverloadedStrings, FlexibleInstances #-}
module Amelie.Pages where

import           Control.Applicative        ((<$>))
import           Control.Applicative        (Applicative(..))
import           Control.Applicative.Error  (Failing(..))
import           Control.Arrow              ((***))
import           Control.Monad              (mplus,ap,unless,liftM)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Reader       (runReaderT,ask)
import           Control.Monad.State        (MonadState)
import           Control.Monad.State        (gets)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans        (lift,liftIO)
import           Data.Char                  (toLower)
import           Data.List                  (find,isInfixOf)
import           Data.Maybe                 (isJust,fromMaybe)
import           System.Directory           (doesFileExist)

import           Codec.Binary.UTF8.String   (decodeString,encodeString)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L (concat)
import           Data.Time.Instances        ()
import           Language.Haskell.HLint     (Suggestion)
import           Language.Haskell.HLint     (hlint)
import           Network.CGI                (CGIResult)
import qualified Network.CGI                as CGI
import           Network.CGI.Monad          (MonadCGI(..))
import           Safe                       (readMay)
import           System.FilePath.Posix      ((</>))
import           Text.Blaze.Renderer.Utf8   (renderHtml)

import           Amelie.DB                  (db)
import qualified Amelie.DB                  as DB
import           Amelie.HTML                 (pasteForm,pastesHtmlTable
                                             ,pasteInfoHtml,pastePasteHtml
                                             ,controlPasteHtml,prevNext
                                             ,pastePreview,hintsToHTML)
import           Amelie.Links               (link)
import           Amelie.Pages.Error         (errorPage)
import           Amelie.Templates           (template,renderTemplate)
import           Amelie.Types                (State(..),Paste(..),
                                              ChansAndLangs,SCGI,
                                              Language(..),Channel(..),
                                              Config(..))
import           Amelie.Utils               (l2s,text,failingToMaybe)

-- | A CGI page of recent pastes and paste form.
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

-- | A CGI page of all pastes.
browsePage :: (MonadState State m,MonadCGI m,MonadIO m,Functor m)
              => [(String,String)] -> ChansAndLangs -> m CGIResult
browsePage params _cl = do
    pastes <- db $ DB.allPastesLimitWithOffset limit page
    let nav = prevNext "browse" limit page (not $ null pastes)
        latestPastes = l2s $ renderHtml $ do
          nav; pastesHtmlTable pastes; nav
    template "All Pastes" "browse" 
             [("pastes",latestPastes)]
             Nothing
  where int i n = maybe i (max 1) $ lookup n params >>= readMay
        limit = int 30 "limit"
        page = int 1 "page"
        

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
renderPaste ps cl@(_,langs) annotation_of paste@Paste{pid,title,language} = 
  if not haskellp
     then renderTemplate "info_paste" $ params ""
     else do
       hints <- hlintHints paste {language=lang}
       let html = l2s $ renderHtml $ hintsToHTML hints
           name | null hints = "info_paste"
                | otherwise  = "info_paste_hlint"
       renderTemplate name $ params html
  where
  haskellp = let l = langName <$> lang
             in l == Just "haskell" || l == Just "literatehaskell"
  params hints =
    [("title",l2s . renderHtml . text $ title)
    ,("info",info)
    ,("paste",paste')]
    ++
    [("hints",hints) | haskellp]
  info = l2s $ renderHtml $ pasteInfoHtml lang' cl paste annotation_of
  paste' = l2s $ renderHtml $ pastePasteHtml paste lang
  lang = (\l->l{langName=map toLower $ langName l}) <$> (lang' `mplus` language)
  lang' = lookup lparam ps >>= \name -> find ((==name) . langName) langs
  lparam = "lang_" ++ show pid

-- | Generate HLint hints for a source.
hlintHints :: (MonadIO m,MonadState State m) =>
              Paste -> m [Suggestion]
hlintHints Paste{pid,content,language=l} = do
    -- This is kind of annoying, I have to prepare a temporary file
    -- for hlint to eat. The alternative is to run hlint as a pipe
    -- if that works, but I don't feel like doing that, nor do I
    -- feel like patching it and then having to depend on a patched
    -- version. Maybe Neil will kindly provide an interface in
    -- which I can provide the source directly.
    pastesDir <- gets $ pastesDir . config
    let ext | (langName <$> l) == Just "haskell" = "hs" 
--            | otherwise                          = "lhs" -- This throws a hlint error.
        path = pastesDir </> show pid ++ "." ++ ext
    exists <- liftIO $ doesFileExist path
    liftIO $ unless exists $ writeFile path content
    filter (not . boring) `liftM` liftIO (hlint [path,"--quiet"])
  where boring = isInfixOf "Parse error" . drop 1 . dropWhile (/=' ') . show

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
  spamTrap <- CGI.getInput "email"
  case spamTrap of
    Just "" -> editCreatePaste params cl
    _       -> CGI.outputNothing
  
-- | The actual, verfied not spam, page.
editCreatePaste :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => [(String,String)] -> ChansAndLangs -> m CGIResult
editCreatePaste params cl = do
  inputs <- map (decodeString *** decodeString) <$> CGI.getInputs
  submitted <- isJust <$> CGI.getInput "submit"
  preview <- isJust <$> CGI.getInput "preview"
  annotation_of <- (>>=readMay) <$> CGI.getInput "annotation_of"
  expirep <- isJust <$> CGI.getInput "expire"
  let (result,_) = pasteForm Nothing cl inputs  
      edit = (lookup "pid" params >>= readMay) `mplus` (pid <$> failingToMaybe result)
  epaste <- case edit of
    Just eid -> db $ DB.pasteById eid cl
    Nothing  -> return Nothing
  let (_,formHtml) = pasteForm epaste cl inputs  
      title' | isJust epaste = "Edit paste" 
             | otherwise     = "New paste"
  case result of
    Failure errs -> pasteErrsOrPreview title' errs formHtml submitted Nothing
    Success paste
      | preview -> pasteErrsOrPreview title' [] formHtml submitted (Just paste)
      | otherwise -> do
        let paste' = paste { expire = expirep }
        pid' <- insertOrUpdate (fromMaybe paste epaste) paste' annotation_of
        let pasteid = fromMaybe pid' annotation_of
        CGI.redirect $ link "paste" $ 
          [("pid",show pasteid)
          ,("title",title paste)]
          ++ [("annotation",show pid') | isJust annotation_of]

-- | Paste page with error(s) or preview.
pasteErrsOrPreview :: (Functor m, MonadState State m, MonadCGI m, MonadIO m)
                   => String -> [String] -> String -> Bool
                   -> Maybe Paste 
                   -> m CGIResult
pasteErrsOrPreview title errs form submitted paste =
    template title "control" [("form",frm),("preview",preview)] Nothing where
  frm = html $ controlPasteHtml title (Just (submitted,errs)) form Nothing
  preview = maybe "" (html . pastePreview) paste
  html = l2s . renderHtml

instance Applicative (ReaderT [(String,String)] Maybe) where
  (<*>) = ap; pure = return

apiPost :: (Functor m,MonadIO m,MonadCGI m,MonadState State m) =>
           [(String,String)] -> ChansAndLangs -> m CGIResult
apiPost _ cl@(cs,ls) = do
  params <- map (decodeString *** decodeString) <$> CGI.getInputs
  let input n = do ps <- ask; lift $ lookup n ps
      paste = flip runReaderT params $
        Paste <$> (input "pid" >>= lift . readMay)
              <*> input "title"
              <*> input "author"
              <*> (input "language" >>= lift . readMay >>= lift . Just . look lid ls)
              <*> (input "channel" >>= lift . readMay >>= lift . Just . look cid cs)
              <*> input "paste"
              <*> return False
              <*> return []
              <*> return Nothing
              <*> return Nothing
  annotation_of <- (>>=readMay) <$> CGI.getInput "annotation_of"
  epaste <- case pid <$> paste of
    Just eid -> db $ DB.pasteById eid cl
    Nothing  -> return Nothing
  case paste of
    Nothing -> CGI.output "invalid parameters"
    Just paste' -> do
      pid' <- insertOrUpdate (fromMaybe paste' epaste) paste' annotation_of
      CGI.output $ show pid'

look acc xs x = find ((==x).acc) xs
  
insertOrUpdate
  :: (MonadState State m, MonadIO m) =>
     Paste -> Paste -> Maybe Int -> m Int
insertOrUpdate _old@Paste{annotation_of=an} paste@Paste{pid} an_of =
  if pid > 0
     then do db $ DB.updatePaste paste{annotation_of=an}; return pid
     else db $ DB.createPaste paste an_of
