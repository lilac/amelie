{-# LANGUAGE DeriveDataTypeable, RecordWildCards, RankNTypes, NamedFieldPuns, 
             DisambiguateRecordFields, TupleSections, GeneralizedNewtypeDeriving, 
             ScopedTypeVariables, FlexibleContexts, FlexibleInstances, 
             OverloadedStrings #-}
module Main where

import           Control.Applicative            (Applicative)
import           Control.Applicative            (pure,(<$>),(<*>),(*>))
import           Control.Applicative.Error      (Failing(..))
import           Control.Arrow                  (second)
import           Control.Monad                  (ap)
import           Control.Monad.Identity         (Identity)
import           Control.Monad.Identity         (runIdentity)
import           Control.Monad.State            (MonadState,StateT)
import           Control.Monad.State            (gets,modify,runStateT)
import           Control.Monad.Trans            (MonadIO)
import           Control.Monad.Trans            (liftIO,lift)
import qualified Data.ByteString.Char8          as B (ByteString)
import qualified Data.ByteString.Char8          as B (readFile,concat,pack)
import qualified Data.ByteString.Lazy           as L (ByteString)
import qualified Data.ByteString.Lazy           as L (toChunks,fromChunks)
import           Data.Char                      (toLower,isDigit,isLetter,toUpper)
import           Data.Data                      (Data,Typeable)
import           Data.List                      (find,nub,intercalate,group,foldl')
import           Data.Maybe                     (listToMaybe,isJust)
import           Data.Monoid                    (mconcat,mempty)
import           System.Directory               (doesFileExist)

import           Data.ByteString.Search         (replace)
import           Data.ConfigFile                hiding (content)
import           Data.List.Split                (splitWhen)
import           Data.Text                      (pack)
import           Database.PostgreSQL.Enumerator (DBM,Session,IsolationLevel(..),ConnectA)
import qualified Database.PostgreSQL.Enumerator as DB
import           Network.CGI                    (CGIResult,CGIT,CGI)
import qualified Network.CGI                    as CGI
import           Network.CGI.Monad              (MonadCGI(..))
import           Network.FastCGI                (runFastCGIorCGI)
import           Safe                           (readMay)
import           System.FilePath                ((</>))
import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Renderer.Utf8       (renderHtml)
import qualified Text.Formlets                  as X
import           Text.Highlighting.Kate         (FormatOption(..))
import qualified Text.Highlighting.Kate         as Kate
import qualified Text.XHtml.Strict              as Html
import qualified Text.XHtml.Strict.Formlets     as XH

-- | A paste.
data Paste =
  Paste { pid      :: Int            -- ^ Database entity id.
        , title    :: Title          -- ^ Title of the paste (limited to 512~).
        , author   :: Author         -- ^ Author(s) of the paste.
        , language :: Maybe Language -- ^ Language (if any) of the paste.
        , channel  :: Maybe Channel  -- ^ Associated IRC channel (if any).
        , content  :: Content        -- ^ Raw content of the paste.
        , tags     :: [Tag]          -- ^ Tags/keywords for the paste.
        } deriving (Typeable,Data,Read,Show)

-- | Title of the paste/thing.
type Title = String

-- | Content of the paste.
type Content = String

-- | A paste tag (or keyword).
type Tag = String

-- | An author of the paste.
type Author = String

-- | The programming language of the paste.
data Language = 
  Language { langName :: String  -- ^ Programming language name.
           , langTitle :: String -- ^ Human friendly (i.e. capitalised) name.
           , lid      :: Int     -- ^ Database entity id.
           } deriving (Typeable,Data,Read,Show)

-- | The channel associated with the paste.
data Channel = 
  Channel { chanName :: String -- ^ Name of the IRC channel (including '#').
          , cid      :: Int    -- ^ Database entity id.
          } deriving (Typeable,Data,Read,Show)

-- | A tuple of channels and languages, used to populate paste objects.
type ChansAndLangs = ([Channel],[Language])

-- | Configuration for the program.
data Config =
  Config { dbconn        :: DBInfo   -- ^ Database host/user/pass.
         , siteTitle     :: String   -- ^ Vanity title of the paste site.
         , pastesPerPage :: Integer  -- ^ Results per page.
         , defaultPage   :: String   -- ^ Default page to show.
         , templateDir   :: FilePath -- ^ Template directory.
         } deriving (Show)

data State =
  State { dbsess :: ConnectA Session 
        , config :: Config
        }

newtype SCGI a = SCGI { runSCGI :: StateT State (CGIT IO) a }
 deriving (Monad,Functor,MonadState State,MonadCGI,MonadIO)
instance Monad m => MonadCGI (StateT State (CGIT m)) where
  cgiGet = lift . cgiGet
  cgiAddHeader = (lift .) . cgiAddHeader

-- | PostgreSQL database information (host,user,pass).
type DBInfo = (String,String,String)

-- | Name of a CGI page.
type PageName = String

-- | Main entry point.
main :: IO ()
main = do
  result <- readConfigFile "amelie.conf"
  case result of
    Left cperr   -> error $ show cperr
    Right config -> runFastCGIorCGI $ CGI.handleErrors $ runPage config router

-- | Route requests to the right page.
router :: SCGI CGIResult
router = do
  def <- gets $ defaultPage . config
  url <- (\n -> if null n then def else n) . dropWhile (=='/') <$> CGI.scriptName
  let name = takeWhile (/='/') url
      params = assocs . splitWhen (=='/') . drop 1 . dropWhile (/='/') $ url
      assocs x = map snd . filter (odd' . fst) . zip [1..] . zip x . tail $ x
        where odd' = odd :: Integer -> Bool
      cls = (db chansAndLangs >>=)
  case name of
    "paste" -> cls $ pastePage params
    "new"   -> cls newPastePage
    _       -> pastesPage

-- | Link to another page with parameters.
link :: PageName -> [(String,String)] -> String
link name params
  | rewritable name = maybe "" (\f -> f name params) $ lookup name rules
  | otherwise = rewriteBasic name params

rewriteBasic :: PageName -> [(String,String)] -> String
rewriteBasic name = slashParts . (name :) . map spec where
    spec (key,value) = key ++ "/" ++ value

-- | Is a page's URL rewritable?
rewritable :: PageName -> Bool
rewritable = isJust . flip lookup rules

-- | Rewrite rules for outgoing links.
rules :: [(PageName,PageName -> [(String,String)] -> String)]
rules = [("paste",rewritePaste)] where
  rewritePaste name params = case params of
    [("pid",pid'),("title",title)] -> slashParts [name,pid',norm title]
    [("pid",pid')]                 -> slashParts [name,pid']
    _ -> rewriteBasic name params
  -- | Normalize a string.
  norm = map toLower . nubseq . replaceUnless '_' valid
  valid c = isDigit c || isLetter c || any (==c) "_"

-- | Join a list of string parts into a slash-separated string.
slashParts :: [String] -> String
slashParts = ('/':) . intercalate "/"

-- | Run a sessioned CGI page with a database connection and configuration.
runPage :: Config -> SCGI CGIResult -> CGI CGIResult
runPage config@Config{dbconn=(host,user,pass)} m = do
  ((),sess) <- liftIO $ DB.withContinuedSession connector (return ())
  let state = State { dbsess = sess, config = config }
  (cgiResult,State{dbsess}) <- flip runStateT state $ runSCGI m
  liftIO $ DB.withSession dbsess $ return ()
  return cgiResult
  where connector = DB.connect [DB.CAhost host,DB.CAuser user,DB.CApassword pass]

-- | Put a rendered page into its corresponding HTML template.
template :: (MonadState State m,MonadCGI m,MonadIO m)
            => Title -> PageName -> H.Html -> m CGIResult
template title' name inner = do
  tempDir <- gets $ templateDir . config
  let (temp,page) = (tempDir </> "template.html",tempDir </> name ++ ".html")
  exists <- liftIO $ (&&) <$> doesFileExist temp 
            <*> doesFileExist page
  if exists
     then do templ <- liftIO $ B.readFile page
             mainTempl <- liftIO $ B.readFile temp
             let innerHtml = B.concat $ L.toChunks $ renderHtml inner
                 params = [("page",templ),("name",B.pack name),
                           ("title",B.pack title'),("inner",innerHtml)] 
             CGI.outputFPS $ fillTemplate params mainTempl
     else CGI.outputFPS $ renderHtml inner

fillTemplate :: [(String,B.ByteString)] -> B.ByteString -> L.ByteString
fillTemplate xs str = L.fromChunks . return $ foldl' rep str xs where
  rep acc (this,with) = B.concat $ L.toChunks $ replace this' with acc where
    this' = B.pack $ "$" ++ map toUpper this ++ "$"

-- | A CGI page of all pastes.
pastesPage :: (MonadState State m,MonadCGI m,MonadIO m,Functor m)
              => m CGIResult
pastesPage =
  db allPastes >>= template "All Pastes" "pastes" . pastesHtml

-- | A CGI page to show a paste.
pastePage :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => [(String,String)]             
             -> ChansAndLangs
             -> m CGIResult
pastePage ps cl =
    case lookup "pid" ps >>= readMay of
      Nothing    -> noPasteGiven
      Just pid'' -> do
        result <- db $ pasteById pid'' cl
        case result of
          Nothing -> noSuchPaste
          Just paste@Paste{title} -> template title "paste" (pasteHtml paste)
  where noSuchPaste = errorPage "Unknown paste."
        noPasteGiven = errorPage "No paste given."

-- | A CGI page to create a new paste.
newPastePage :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => ChansAndLangs -> m CGIResult
newPastePage cl = do
  inputs <- CGI.getInputs
  submitted <- isJust <$> CGI.getInput "submit"
  let (result,formHtml) = pasteForm cl inputs
  case result of
    Failure errs -> pageWithErrors errs formHtml submitted
    Success paste -> do pid' <- db $ createPaste paste
                        -- TODO: redirect to the paste page.
                        CGI.output $ "Paste " ++ show pid' ++ " created!"
  where pageWithErrors errs form submitted = template "New paste" "new" $ do
          if submitted
            then H.ul . mconcat . map (H.li . H.text . pack) $ errs
            else mempty
          H.form ! A.method "post" $ do
            H.preEscapedString form
            H.input ! A.type_ "submit" ! A.value "Create Paste" ! A.class_ "submit"
            H.input ! A.type_ "hidden" ! A.value "true" ! A.name "submit"

-- | A friendly error page.
errorPage :: MonadCGI m => String -> m CGIResult
errorPage = CGI.output

-- | HTML representation of a pastes list.
pastesHtml :: [Paste] -> H.Html
pastesHtml = table . H.tbody . mconcat . map pasteRowHtml where
  table tbody = H.table $ do thead; tbody
  thead = H.thead $ H.tr $ mconcat $ map (H.th . text) fields
  fields = ["Title","Author","Channel","Language"]
  pasteRowHtml Paste{..} = H.tr $ do
    H.td $ H.a ! A.href (H.stringValue url) $ text title
    td author
    td $ maybe "-" chanName channel
    td $ maybe "-" langName language
    where td = H.td . H.text . pack
          url = link "paste" [("pid",show pid),("title",title)]
  text = H.text . pack

-- | HTML representation of a paste.
pasteHtml :: Paste -> H.Html
pasteHtml paste@Paste{..} = do
  H.style $ text highlightCSS
  H.dl $ do def "Author" $ text author
            def "Channel" $ text $ maybe "-" chanName channel
  H.div $ H.preEscapedString $ pasteHighlightedHtml paste
  where def t dd = do H.dt $ text t; H.dd dd
        text = H.text . pack

-- | An identity monad for running forms, with Applicative instance.
newtype RunForm a = RF { runForm :: Identity a } deriving (Monad,Functor)
instance Applicative RunForm where (<*>) = ap; pure = return

-- | A form for submitting a new paste.
pasteForm :: ChansAndLangs -> [(String,String)] -> (Failing Paste,String)
pasteForm (chans,langs) inputs = runIdentity $ runForm resultAndHtml where
  resultAndHtml = (,Html.renderHtmlFragment html) <$> run
  (run,html,_) = X.runFormState env form
  env = map (second Left) inputs
  form = Paste <$> pure 0
               <*> label "Title"    nempty (XH.input Nothing)
               <*> label "Author"   nempty (XH.input Nothing)
               <*> label "Language" we langInput
               <*> label "Channel"  we chanInput
               <*> label "Paste"    nempty (clean <$> pasteInput)
               <*> pure []
  langInput = lookupLang <$> XH.select [] (map makeLangChoice langs) Nothing where
    lookupLang lid' = find ((==lid').lid) langs
    makeLangChoice Language{lid,langTitle} = (lid,langTitle)
  chanInput = lookupChan <$> XH.select [] (map makeChanChoice chans) Nothing where
    lookupChan cid' = find ((==cid').cid) chans
    makeChanChoice Channel{cid,chanName} = (cid,chanName)
  pasteInput = XH.textarea (Just 30) (Just 50) Nothing
  clean = filter (/='\r') -- For some reason highlighting-kate counts \r\n as 2 lines.
  nempty = not . null
  we = const True

-- | Label an input and apply a predicate to it for making inputs required.
label :: (Show a,Monad m,Applicative m) =>
          String -> (a -> Bool) -> X.Form Html.Html m a -> X.Form Html.Html m a
label caption p inp = XH.label (caption ++ ": ") *> (inp `X.check` X.ensure p msg) where
  msg = caption ++ ": must be provided"

-- | Highlighting CSS.
highlightCSS :: String
highlightCSS = Kate.defaultHighlightingCss

-- | Syntax highlight a paste.
pasteHighlightedHtml :: Paste -> String
pasteHighlightedHtml Paste{content,language} =
  let langName' = maybe "" langName language
  in case Kate.highlightAs (map toLower langName') content of
       Left _err -> content
       Right slines -> show $
         Kate.formatAsXHtml [OptNumberLines,OptLineAnchors] langName' slines

-- | Attempt to read the configuration file from a given file path.
readConfigFile :: FilePath -> IO (Either CPError Config)
readConfigFile p = do
  exists <- doesFileExist p
  if exists
     then getConfig <$> readFile p
     else return $ Left (OtherProblem er,er) where er = "File not found"

-- | Read the config file contents into a Config object.
getConfig :: String -> Either CPError Config
getConfig config = do
  c <- readstring emptyCP config
  [host,user,pass] <- mapM (get c "POSTGRESQL") ["host","user","password"]
  [title,perpage,def,temps]  <- mapM (get c "PRESENTATION")
                                     ["title","perpage","defaultpage","templates"]
  return Config { dbconn        = (host,user,pass) 
                , siteTitle     = title
                , pastesPerPage = read perpage
                , defaultPage   = def
                , templateDir   = temps
                }

-- | With a connection to the database.
withDB  :: (Typeable a) => 
           DBInfo -> (forall mark. DBM mark Session a) -> IO a
withDB (host,user,pass) = DB.withSession $
  DB.connect [DB.CAhost host,DB.CAuser user,DB.CApassword pass]

-- | Run a database computation with the current database session.
db :: (Typeable a,MonadIO m,MonadState State m)
      => (forall mark. DBM mark Session a) -> m a
db m = do sess <- gets dbsess
          (a,sess') <- liftIO $ DB.withContinuedSession sess m
          modify $ \s -> s { dbsess = sess' }
          return a

-- | Run a database computation within a transaction.
trans :: DBM mark Session a -> DBM mark Session a
trans = DB.withTransaction Serialisable

-- | Create a new paste.
createPaste :: Paste -> DBM mark Session Int
createPaste paste =
  trans $ do
    insertPaste paste
    DB.doQuery (DB.sql "select last_value from paste_id_seq")
      (\pid' nil -> DB.result' (pid'+nil)) (0::Int)

-- | Insert a new paste.
insertPaste :: Paste -> DBM mark Session ()
insertPaste Paste{..} = DB.execDDL (DB.cmdbind stmt params) where
  stmt = unwords ["insert into paste (" ++ fieldsSpec ++ ")"
                 ,"values (" ++ values ++ ")"]
  fieldsSpec = intercalate "," $ map fst fields
  params = map snd fields
  values = intercalate "," $ map (const "?") fields
  fields = [("title",DB.bindP title)
           ,("content",DB.bindP content)
           ,("tags",DB.bindP $ intercalate "," tags)
           ,("author",DB.bindP author)
           ,("language",DB.bindP $ fmap lid language)
           ,("channel",DB.bindP $ fmap cid channel)]

-- | Channels and languages.
chansAndLangs :: DBM mark Session ([Channel],[Language])
chansAndLangs = do
  c <- allChannels
  l <- allLanguages
  return (c,l)
  
-- | Retrieve all pastes.
allPastes :: DBM mark Session [Paste]
allPastes = do
  cl <- chansAndLangs
  pastesByQuery cl ""

-- | Retrieve all channels.
allChannels :: DBM mark Session [Channel]
allChannels = DB.doQuery (DB.sql "select id,title from channel") makeChannel [] where
  makeChannel cid' chanName' xs = DB.result' (chan:xs) where
    chan = Channel { chanName = chanName', cid = cid' }

-- | Retrieve all languages.
allLanguages :: DBM mark Session [Language]
allLanguages = DB.doQuery (DB.sql "select id,name,title from language") makeLanguage [] where
  makeLanguage lid' langName' langTitle' xs = DB.result' (lang:xs) where
    lang = Language { langName = langName', lid = lid', langTitle = langTitle' }

-- | Retrieve pastes by channel.
pastesByChannel :: Int -> ChansAndLangs -> DBM mark Session [Paste]
pastesByChannel cid cl = pastesByQuery cl $ "where channel = " ++ show cid

-- | Retrieve pastes by substring (simple search).
pastesBySubstring :: String -> ChansAndLangs -> DBM mark Session [Paste]
pastesBySubstring q cl = pastesByQuery cl $ "where FALSE" ++ search where 
  search = case pastesSearchString q of
    "" -> ""
    cond -> " or " ++ cond

-- | A WHERE condition string for searching by a set of words ('terms').
pastesSearchString :: String -> String
pastesSearchString = cond where
  cond = intercalate " or " . map checkTerm . terms
  terms = nub . map sterilize . filter ((/="") . filter valid) . splitWhen (==' ')
  checkTerm term = "title like '%" ++ term ++ "%'"
  sterilize = replaceUnless '_' valid
  valid c = isDigit c || isLetter c || any (==c) ".-"

-- | Retrieve paste by its primary key.
pasteById :: Int -> ChansAndLangs -> DBM mark Session (Maybe Paste)
pasteById pid' cl = listToMaybe <$> pastesByQuery cl ("where id = " ++ show pid')

-- | Return a list of pastes by the query.
pastesByQuery :: ChansAndLangs -> String -> DBM mark Session [Paste]
pastesByQuery (chans,langs) cond = DB.doQuery (DB.sql query) makePaste [] where
  query = "select " ++ fields ++ " from paste " ++ cond
  fields = "id,title,content,tags,author,language,channel"
  makePaste pid' title' content' tags' author' lang' chan' xs =
    DB.result' (paste:xs) where
      paste = Paste { pid = pid'
                    , title    = title'
                    , content  = content' 
                    , tags     = maybe [] (splitWhen (==',')) tags'
                    , author   = author'
                    , language = lang' >>= \lid' -> find ((==lid').lid) langs
                    , channel  = chan' >>= \cid' -> find ((==cid').cid) chans
                    }

-- | Replace elements of a list with a value, if a predicate is satisfied.
replaceUnless :: a -> (a -> Bool) -> [a] -> [a]
replaceUnless with p = map (\a -> if not (p a) then with else a)

-- | Remove duplicate subsequent elements of a list.
nubseq :: Eq a => [a] -> [a]
nubseq = map head . group
