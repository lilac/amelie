{-# LANGUAGE DeriveDataTypeable, RecordWildCards, RankNTypes, NamedFieldPuns, 
             DisambiguateRecordFields, TupleSections, GeneralizedNewtypeDeriving, 
             ScopedTypeVariables, FlexibleContexts, FlexibleInstances, 
             OverloadedStrings #-}
module Main where

import           Control.Applicative            (Applicative)
import           Control.Applicative            (pure,(<$>),(<*>),(*>))
import           Control.Applicative.Error      (Failing(..))
import           Control.Arrow                  (second,(***))
import           Control.Monad                  (ap)
import           Control.Monad                  (mplus)
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
import           Data.Char                      (toLower,toUpper)
import           Data.Data                      (Data,Typeable)
import           Data.List                      (find,nub,intercalate,foldl',union)
import           Data.Maybe                     (listToMaybe,isJust,fromMaybe)
import           Data.Monoid                    (mconcat,mempty)
import           Data.Time                      (UTCTime,formatTime)
import           System.Directory               (doesFileExist)
import           System.Locale                  (defaultTimeLocale)

import           Codec.Binary.UTF8.String       (decodeString,encodeString)
import           Data.ByteString.Search         (replace)
import           Data.ConfigFile                hiding (content)
import           Data.List.Split                (splitWhen)
import qualified Data.List.Utils                as List (replace)
import           Data.Text                      (pack)
import           Data.Time.Instances            ()
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
import           Text.XHtml.Strict              ((<<))
import qualified Text.XHtml.Strict              as Html
import qualified Text.XHtml.Strict.Formlets     as XH

-- | A paste.
data Paste =
  Paste { pid      :: Int             -- ^ Database entity id.
        , title    :: Title           -- ^ Title of the paste (limited to 512~).
        , author   :: Author          -- ^ Author(s) of the paste.
        , language :: Maybe Language  -- ^ Language (if any) of the paste.
        , channel  :: Maybe Channel   -- ^ Associated IRC channel (if any).
        , content  :: Content         -- ^ Raw content of the paste.
        , tags     :: [Tag]           -- ^ Tags/keywords for the paste.
        , created  :: Maybe UTCTime   -- ^ When the paste was created.
        } deriving (Typeable,Data,Show)

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
         , analytics     :: String   -- ^ Analytics profile id.
         } deriving (Show)

-- | Site database/configuration state.
data State =
  State { dbsess :: ConnectA Session 
        , config :: Config
        }

-- | Our web/database transformer.
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
  inputs <- CGI.getInputs
  let name = takeWhile (/='/') url
      urlParams = assocs . splitWhen (=='/') . drop 1 . dropWhile (/='/') $ url
      params = inputs `union` urlParams
      assocs x = map snd . filter (odd' . fst) . zip [1..] . zip x . tail $ x
        where odd' = odd :: Integer -> Bool
      cls = (db chansAndLangs >>=)
  CGI.setHeader "Content-Type" "text/html; charset=UTF-8"
  case name of
    "paste" -> cls $ pastePage params
    "raw"   -> cls $ rawPastePage params
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
rules = [("paste",rewritePaste),("raw",rewritePaste)] where
  rewritePaste name params = case params of
    [("pid",pid'),("title",title)] 
      | name == "raw"   -> slashParts [name,pid',norm title]
      | name == "paste" -> slashParts [pid',norm title]
    [("pid",pid')]                 
      | name == "raw"   -> slashParts [name,pid']
      | name == "paste" -> slashParts [pid']
    _ -> rewriteBasic name params
  -- | Normalize a string.
  norm = map toLower . replaceUnless '_' valid
  valid c = any (==toLower c) $ "_" ++ ['a'..'z'] ++ ['0'..'9']

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
            => Title -> PageName -> [(String,B.ByteString)] -> Maybe H.Html
            -> m CGIResult
template title' name ps inner = do
  tempDir <- gets $ templateDir . config
  analytics <- gets $ analytics . config
  let (temp,page) = (tempDir </> "template.html",tempDir </> name ++ ".html")
  exists <- liftIO $ (&&) <$> doesFileExist temp 
            <*> doesFileExist page
  if exists
     then do templ <- liftIO $ B.readFile page
             mainTempl <- liftIO $ B.readFile temp
             let params = [("page",templ)
                          ,("name",B.pack name)
                          ,("title",B.pack $ encodeString title')
                          ,("analytics",B.pack analytics)]
                          ++ ps ++ renderedHtml
             CGI.outputFPS $ fillTemplate params mainTempl
     else maybe (errorPage $ "No template for " ++ name) 
                (CGI.outputFPS . renderHtml) 
                inner
    where renderedHtml = case inner of
            Just in' -> [("inner",l2s $ renderHtml in')]
            Nothing -> []

-- | Fill a template's parameters.
fillTemplate :: [(String,B.ByteString)] -> B.ByteString -> L.ByteString
fillTemplate xs str = L.fromChunks . return $ foldl' rep str xs where
  rep acc (this,with) = l2s $ replace this' with acc where
    this' = B.pack $ "<$" ++ map toUpper this ++ "$>"

-- | A CGI page of all pastes.
pastesPage :: (MonadState State m,MonadCGI m,MonadIO m,Functor m)
              => m CGIResult
pastesPage = do
  pastes <- db $ allPastesLimitBy 20
  cl <- db chansAndLangs
  let latestPastes = l2s $ renderHtml $ pastesHtmlTable pastes
      newPasteForm = l2s $ renderHtml $
                     newPasteHtml Nothing $ snd $ pasteForm cl []
  template "All Pastes" "pastes" 
           [("latest_pastes",latestPastes)
           ,("new_paste_form",newPasteForm)]
           Nothing

-- | Render a pretty highlighted paste with info.
pastePage :: [(String, String)] -> ChansAndLangs -> SCGI CGIResult
pastePage = asPastePage $ \ps paste@Paste{title} ->
  let info = l2s $ renderHtml $ pasteInfoHtml paste
      paste' = l2s $ renderHtml $ pastePasteHtml paste $ lookup "lang" ps
  in template title "paste" [("info",info),("paste",paste')] Nothing

-- | Render a raw paste.
rawPastePage :: [(String, String)] -> ChansAndLangs -> SCGI CGIResult
rawPastePage = asPastePage $ \_ Paste{content} -> do
  CGI.setHeader "Content-Type" "text/plain; charset=UTF-8"
  CGI.output $ encodeString content

-- | A CGI page to show a paste.
asPastePage :: (Functor m,MonadIO m,MonadCGI m,MonadState State m)
             => ([(String,String)] -> Paste -> m CGIResult)
             -> [(String,String)]             
             -> ChansAndLangs
             -> m CGIResult
asPastePage run ps cl =
    case lookup "pid" ps >>= readMay of
      Nothing    -> noPasteGiven
      Just pid'' -> do
        result <- db $ pasteById pid'' cl
        case result of
          Nothing    -> noSuchPaste
          Just paste -> run ps paste
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
    Success paste -> do pid' <- db $ createPaste paste
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

-- | A friendly error page.
errorPage :: MonadCGI m => String -> m CGIResult
errorPage = CGI.output

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
  fields = ["Title","Author","Channel","Language"]
  pasteRowHtml Paste{..} = H.tr $ do
    H.td $ H.a ! A.href (H.stringValue url) $ text title
    td author
    td $ maybe "-" chanName channel
    td $ maybe "-" langTitle language
    where td = H.td . H.text . pack
          url = link "paste" [("pid",show pid),("title",title)]

-- | Paste info of a paste.
pasteInfoHtml :: Paste -> H.Html
pasteInfoHtml Paste{..} = 
  H.ul $ do def "Paste" $ href (self "paste") $ text $ "#" ++ show pid
            def "Author" $ text author
            maybe mempty (def "Channel" . text . chanName) channel
            def "Created" $ H.span ! A.id "created" $ text $ format created
            def "Raw" $ href (self "raw") $ text "View raw file"
  where def t dd = H.li $ do H.strong $ text $ t ++ ":"; H.span dd
        format = maybe "" $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"
        self typ = link typ [("pid",show pid),("title",title)]
        href l c = H.a ! A.href (H.stringValue l) $ c

-- | Paste HTML of a paste.
pastePasteHtml :: Paste -> Maybe String -> H.Html
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
                 <*> label "Language" we langInput
                 <*> label "Channel"  we chanInput
                 <*> label "Paste"    nempty (clean <$> pasteInput)
                 <*> pure []
                 <*> pure Nothing
  langInput = lookupLang <$> XH.select [] (empty ++ map makeLangChoice langs) Nothing where
    lookupLang lid' = find ((==lid').lid) langs
    makeLangChoice Language{lid,langTitle} = (lid,langTitle)
  chanInput = lookupChan <$> XH.select [] (empty ++ map makeChanChoice chans) Nothing where
    lookupChan cid' = find ((==cid').cid) chans
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

-- | Highlighting CSS.
highlightCSS :: String
highlightCSS = Kate.defaultHighlightingCss

-- | Syntax highlight a paste.
pasteHighlightedHtml :: Paste -> Maybe String -> Maybe String
pasteHighlightedHtml Paste{content,language} lang =
  let langName' = fromMaybe "" $ lang `mplus` fmap langName language
  in case Kate.highlightAs (map toLower langName') content of
       Left _err -> Nothing
       Right slines -> Just $ show $
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
  [analytics] <- mapM (get c "THIRDPARTY") ["analytics"]
  return Config { dbconn        = (host,user,pass) 
                , siteTitle     = title
                , pastesPerPage = read perpage
                , defaultPage   = def
                , templateDir   = temps
                , analytics     = analytics
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

-- | Retrieve all pastes limit by a row count.
allPastesLimitBy :: Integer -> DBM mark Session [Paste]
allPastesLimitBy limit = do
  cl <- chansAndLangs
  reverse <$> pastesByQuery cl (" ORDER BY id DESC LIMIT " ++ show limit)

-- | Retrieve all channels.
allChannels :: DBM mark Session [Channel]
allChannels = DB.doQuery (DB.sql q) makeChannel [] where
  q = "select id,title from channel order by title desc"
  makeChannel cid' chanName' xs = DB.result' (chan:xs) where
    chan = Channel { chanName = chanName', cid = cid' }

-- | Retrieve all languages.
allLanguages :: DBM mark Session [Language]
allLanguages = DB.doQuery (DB.sql q) makeLanguage [] where
  q = "select id,name,title from language order by title desc"
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
  valid c = any (==toLower c) $ "_" ++ ['a'..'z'] ++ ['0'..'9']

-- | Retrieve paste by its primary key.
pasteById :: Int -> ChansAndLangs -> DBM mark Session (Maybe Paste)
pasteById pid' cl = listToMaybe <$> pastesByQuery cl ("where id = " ++ show pid')

-- | Return a list of pastes by the query.
pastesByQuery :: ChansAndLangs -> String -> DBM mark Session [Paste]
pastesByQuery (chans,langs) cond = DB.doQuery (DB.sql query) makePaste [] where
  query = "select " ++ fields ++ " from paste " ++ cond
  fields = "id,title,content,tags,author,language,channel,created at time zone 'utc'"
  makePaste pid' title' content' tags' author' lang' chan' created' xs =
    DB.result' (paste:xs) where
      paste = Paste { pid = pid'
                    , title    = title'
                    , content  = content' 
                    , tags     = maybe [] (splitWhen (==',')) tags'
                    , author   = author'
                    , language = lang' >>= \lid' -> find ((==lid').lid) langs
                    , channel  = chan' >>= \cid' -> find ((==cid').cid) chans
                    , created  = Just created'
                    }

-- | Replace elements of a list with a value, if a predicate is satisfied.
replaceUnless :: a -> (a -> Bool) -> [a] -> [a]
replaceUnless with p = map (\a -> if not (p a) then with else a)

-- | Convert a lazy bytestring to a strict bytestring.
l2s :: L.ByteString -> B.ByteString
l2s  = B.concat . L.toChunks

-- | Make a HTML text object.
text :: String -> H.Html
text = H.text . pack
