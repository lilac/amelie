 {-# LANGUAGE NamedFieldPuns, FlexibleContexts, OverloadedStrings, FlexibleInstances, RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Amelie.Pages where

import qualified Data.ByteString.Lazy.UTF8 as UTF8 (toString)
import           Amelie.Highlight
import           Control.Applicative        ((<$>))
import           Control.Applicative.Error  (Failing(..))
import           Control.Arrow              ((***))
import           Control.Monad              (mplus,unless,liftM)
import           Control.Monad.State        (MonadState)
import           Control.Monad.State        (gets)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans        (liftIO)
import           Data.Char
import           Data.Char                  (toLower)
import           Data.List                  (find,isInfixOf)
import           Data.List.Higher           (list)
import           Data.Maybe
import           Data.String
import           System.Directory           (doesFileExist)
import           Text.Atom.Feed
import           Text.Atom.Feed.Export
import qualified Text.Blaze.Html5           as H
import qualified Text.Blaze.Html5           as H
import           Text.XML.Light.Output

import           Codec.Binary.UTF8.String   (decodeString,encodeString)
import qualified Data.ByteString            as B (ByteString)
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
import           Text.JSON                  (encode,showJSON,makeObj)

import           Amelie.DB                  (db)
import qualified Amelie.DB                  as DB
import           Amelie.HTML                 (pasteForm,pastesHtmlTable
                                             ,pasteInfoHtml,pastePasteHtml
                                             ,controlPasteHtml,prevNext
                                             ,pastePreview,hintsToHTML,codePadHTML)
import           Amelie.Links               (link)
import           Amelie.Pages.Error         (errorPage)
import           Amelie.Templates           (template,renderTemplate,renderedTemplate)
import           Amelie.Types
import           Amelie.Utils               (l2s,text,failingToMaybe)
import qualified Web.Codepad                as Codepad

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
        
-- | JSON resource to get the output of the code on-demand.
codeOutput :: [(String,String)] -> SCGI CGIResult
codeOutput ps = do
  case lookup "pid" ps >>= readMay of
    Nothing  -> CGI.outputNothing
    Just pid -> do
      paste <- db $ DB.pasteById pid ([],[])
      case paste of
        Just Paste{output=Just output} ->
          CGI.output $ encode $
            makeObj [("success",showJSON True),("output",showJSON output)]
        _ -> CGI.output $ encode $ makeObj [("success",showJSON False)]

-- | Render a pretty highlighted paste with info.
pastePage :: [(String, String)] -> ChansAndLangs -> SCGI CGIResult
pastePage = asPastePage maybeGenOutput where
  maybeGenOutput ps cl paste@Paste{output=curOutput} = do
    let run = isJust $ lookup "run" ps
    if False && run && isNothing curOutput
       then do output <- getPasteOutput paste
               page ps cl paste { output = output }
       else page ps cl paste
  page ps cl main@Paste{pid=main_id,title=mainTitle,annotation_of} = do
      pastes <- db $ DB.pastesByAnnotationOf main_id cl
      annotated <- case annotation_of of
        Just anid -> db $ DB.pasteById anid cl
        Nothing   -> return Nothing      
      let paste_renders = renderPaste ps cl annotated main
                          : map (renderPaste ps cl (Just main)) pastes
      rendered <- sequence <$> sequence paste_renders
      annotateForm <- getAnnotateForm cl main
      case rendered of
        Right (html:anns) -> do
          as <- renderAnnotations anns
          let params = [("paste",l2s html),("annotations",l2s as)
                       ,("annotate",l2s annotateForm)]
          template mainTitle "paste_and_annotations" params Nothing
        Right _     -> errorPage "No paste."
        Left err    -> errorPage err
  renderAnnotations =
    list (return "") $
      \as -> renderedTemplate "annotations" [("pastes",l2s $ L.concat as)]

-- | Get the paste output.
getPasteOutput :: Paste -> SCGI (Maybe String)
getPasteOutput paste@Paste{content,language} = do
  case language of
    Nothing                    -> return Nothing
    Just (Language{langTitle}) -> do
      res <- Codepad.pasteAndRun content langTitle True
      case res of
        Nothing         -> return Nothing
        Just (_,output) -> do
          db $ DB.updatePaste paste { output = Just output }
          return $ Just output

-- | Annotation form.
getAnnotateForm :: ChansAndLangs -> Paste -> SCGI L.ByteString
getAnnotateForm cl Paste{pid=annotation_of} = do
  inputs <- map (decodeString *** decodeString) <$> CGI.getInputs
  let (_,formHtml) = pasteForm Nothing cl inputs
  return $ renderHtml $ controlPasteHtml "Annotate paste"
             Nothing formHtml (Just annotation_of)

-- | Try to a paste info and content to HTML string.
renderPaste :: (Functor m,MonadIO m, MonadState State m)
               => [(String, String)] -> ChansAndLangs
               -> Maybe Paste
               -> Paste
               -> m (Either String L.ByteString)
renderPaste ps cl aof paste@Paste{title} = do
    let langs = "C C++ D Haskell Lua OCaml PHP Perl Python Ruby Scheme Tcl"
    let (info,paste',lang) = pasteAndInfo ps cl aof paste $ words langs
    hints <- renderHints paste {language=lang}
    output <- renderOutput paste
    let params = [("title",l2s . renderHtml . text $ title)
                 ,("info",info)
                 ,("paste",paste')
                 ,("hints",hints)
                 ,("output",output)]
    renderTemplate "paste" $ params

-- | Render the code output via CodePad.
renderOutput :: (MonadState State m,Functor m,MonadIO m) => Paste -> m B.ByteString
renderOutput Paste{output=Nothing} = return ""
renderOutput Paste{output=Just output}
  | null output = return ""
  | otherwise = l2s <$> renderedTemplate "output" params
    where params = [("output",l2s $ renderHtml $ codePadHTML output)]

-- | Render the HLint hints if there are any.
renderHints :: (MonadState State m,Functor m,MonadIO m) => Paste -> m B.ByteString
renderHints paste@Paste{language}
  | langIsHaskell language  = do
    hints <- hlintHints paste
    let params = [("hints",l2s $ renderHtml $ hintsToHTML hints)]
    if null hints
      then return ""
      else l2s <$> renderedTemplate "hints" params
  | otherwise = return ""

-- | Is the language some variation of Haskell?
langIsHaskell :: Maybe Language -> Bool
langIsHaskell =
  (`elem` map (Just . map toLower) ["haskell","literatehaskell"]) . fmap langName

-- | Generate the paste and paste info HTML.
pasteAndInfo :: [(String,String)] -> ChansAndLangs -> Maybe Paste -> Paste
                -> [Codepad.LangName]
             -> (B.ByteString,B.ByteString,Maybe Language)
pasteAndInfo ps cl@(_,langs) aof paste@Paste{pid,language,output} supportedLangs =
    (info,paste',lang) where 
  info = l2s $ renderHtml $ pasteInfoHtml lang' cl paste aof run
  paste' = l2s $ renderHtml $ pastePasteHtml paste lang
  lang = (\l->l{langName=map toLower $ langName l}) <$> (lang' `mplus` language)
  lang' = lookup lparam ps >>= \name -> find ((==name) . langName) langs
  lparam = "lang_" ++ show pid
  run = isNothing output &&
        any (==maybe "" langTitle language) supportedLangs

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
            | otherwise                          = "lhs" -- This throws a hlint error.
        path = pastesDir </> show pid ++ "." ++ ext
    exists <- liftIO $ doesFileExist path
    liftIO $ unless exists $ writeFile path ('\n' : content)
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
                   => String -> [String] -> H.Html -> Bool
                   -> Maybe Paste 
                   -> m CGIResult
pasteErrsOrPreview title errs form submitted paste =
    template title "control" [("form",frm),("preview",preview)] Nothing where
  frm = html $ controlPasteHtml title (Just (submitted,errs)) form Nothing
  preview = maybe "" (html . pastePreview) paste
  html = l2s . renderHtml

insertOrUpdate
  :: (MonadState State m, MonadIO m) =>
     Paste -> Paste -> Maybe Int -> m Int
insertOrUpdate _old@Paste{annotation_of=an} paste@Paste{pid} an_of =
  if pid > 0
     then do db $ DB.updatePaste paste{annotation_of=an}; return pid
     else db $ DB.createPaste paste an_of

-- | Pastes feed.
feed :: (Functor m,MonadIO m,MonadState State m,MonadCGI m) => m CGIResult
feed = do
  chan <- CGI.getInput "chan"
  pastes <- db $ DB.allPastesLimitByWChan 30 (fromMaybe "" chan)
  case pastes of
    []     -> CGI.output "No pastes!"
    pastes -> CGI.output $ showTopElement $ xmlFeed $ feed pastes

    where feed pastes = atom {
              feedEntries = map entry pastes
            } where atom = nullFeed "" title date
                    title = TextString "hpaste pastes"
                    date = fromMaybe "" $ listToMaybe $ 
                           map (show.created) pastes
          entry paste@Paste{..} = entry {
            entryLinks = [(nullLink $ "http://hpaste.org/" ++ show pid) {
                            linkRel = Just (Right "alternate")
                          }]
           ,entryContent = Just $ HTMLContent $
             maybe (UTF8.toString $ renderHtml $ H.text $ fromString content)
                   id
                   html
           }
            where entry = nullEntry (show pid) title' (show created)
                  title' = TextString $
                    clean author ++ " pasted " ++ show title
                  clean = filter irc where
                    irc c = isDigit c || isLetter c || elem c "_-{}[]^"
                  html = fmap (style++)
                              (pasteHighlightedHtml paste language)
                      where style = "<style>" ++ highlightCSS ++ "</style>"
