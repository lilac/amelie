-- | Simple pasting API for CodePad.org by Chris Done <chrisdone@gmail.com>
module Web.Codepad where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (MonadIO,liftIO)
import           Data.Char           (isSpace)
import           Data.Monoid         (mconcat)
import qualified Network.Curl        as C
import           Network.URI         (escapeURIString)
import           Text.HTML.TagSoup   (Tag(..),parseTags)

-- | A URL.
type URL = String

-- | Code to be pasted.
type Code = String

-- | A CodePad paste id e.g. HZPquoIO.
type PasteId = String

-- | Paste output
type PasteOutput = String

-- | Alias for supported CodePad languages.
type LangName = String

-- | CodePad's domain.
codepadUrl :: URL
codepadUrl = "http://codepad.org/"

-- | A CodePad URL of a page containing a list of supported languages.
codepadLangsURL :: URL
codepadLangsURL = "http://hpaste.codepad.org/"

-- | Make a CodePad URL for the given paste id.
pasteURL :: PasteId -- ^ ID of the CodePad paste to construct a URL for. 
         -> URL     -- ^ A CodePad URL to the paste.
pasteURL pid = codepadUrl ++ pid

-- | Paste some code and get the run output too.
pasteAndRun :: MonadIO m
            => Code                             -- ^ Code to paste. 
            -> LangName                         -- ^ Language of the code. 
            -> Bool                             -- ^ Private? 
            -> m (Maybe (PasteId,PasteOutput)) -- ^ The paste id and the run output.
pasteAndRun code lang private = do
  result <- pasteCode code lang True private
  case result of
    Nothing  -> return Nothing
    Just pid -> do
      out <- pasteOutput pid
      case out of
        Nothing     -> return Nothing
        Just output -> return $ Just (pid,output)

-- | Perform a paste.
pasteCode :: MonadIO m
          => Code               -- ^ Code to paste. 
          -> LangName           -- ^ Language of the code. 
          -> Bool               -- ^ Run it?
          -> Bool               -- ^ Private?
          -> m (Maybe PasteId) -- ^ The pasted id.
pasteCode code lang run private = do
    r <- liftIO $ C.withCurlDo $ getResponse codepadUrl [C.CurlPostFields assocs] 
    if C.respStatus r == 302
       then return $ getId <$> lookup "Location" (C.respHeaders r)
       else return Nothing
  where getResponse :: C.URLString -> [C.CurlOption]
                       -> IO (C.CurlResponse_ [(String,String)] String)
        getResponse = C.curlGetResponse_
        getId = reverse . takeWhile (/='/') . reverse
        assocs = ["code=" ++ encode code
                 ,"lang=" ++ encode lang
                 ,"run=" ++ show run
                 ,"private=" ++ show private
                 ,"submit=Submit"]
        encode = escapeURIString (const False)

-- | Get the run output for a paste id.
pasteOutput :: MonadIO m
            => PasteId                -- ^ A CodePad paste id.
            -> m (Maybe PasteOutput) -- ^ Maybe the run output of that paste.
pasteOutput pid = do
  (code,t) <- liftIO $ C.withCurlDo $ C.curlGetString_ (pasteURL pid) []
  case code of
    C.CurlOK -> return $ parseOutput t
    _        -> return Nothing

-- | Get the list of supported languages.
supportedLangs :: MonadIO m => m (Maybe [LangName])
supportedLangs = do
  (code,t) <- liftIO $ C.withCurlDo $ C.curlGetString_ codepadLangsURL []
  case code of
    C.CurlOK -> return $ Just $ parseLangs t
    _        -> return Nothing  

-- | Get the paste output.
parseOutput :: String            -- ^ Parse a paste page for the output.
            -> Maybe PasteOutput -- ^ Maybe the paste output.
parseOutput = toHeading . parseTags where
  toHeading (TagOpen "span" [("class","heading")]
             :TagText "Output:":xs) = skipLines xs
  toHeading (_:xs)                  = toHeading xs
  toHeading []                      = Nothing
  skipLines (TagClose "pre":xs) = toPre xs
  skipLines (_:xs)              = skipLines xs
  skipLines []                  = Nothing
  toPre (TagOpen "pre" _:xs) = cleanUp <$> mconcat (collect xs)
  toPre (_:xs)               = toPre xs
  toPre []                   = Nothing
  collect (TagText t:xs)     = Just t : collect xs
  collect (TagClose "pre":_) = []
  collect (_:xs)             = collect xs
  collect []                 = []
  -- Codepad adds extra space at the start and end of the pre.
  cleanUp s = dropWhile isSpace $ take (length s-1) s

-- | Extract the available languages from the CodePad page.
parseLangs :: String     -- ^ The HTML page. 
           -> [LangName] -- ^ Any parsed language names.
parseLangs = toSelect . parseTags where 
  toSelect (TagOpen "select" ((_,"lang"):_):xs) = options xs
  toSelect (_:xs)                               = toSelect xs
  toSelect []                                   = []
  options (TagOpen "option" _:TagText lang:xs) = lang : options xs
  options (_:xs)                               = options xs
  options []                                   = []
