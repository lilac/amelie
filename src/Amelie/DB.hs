{-# LANGUAGE DeriveDataTypeable, RecordWildCards, RankNTypes, NamedFieldPuns, 
             DisambiguateRecordFields, TupleSections, GeneralizedNewtypeDeriving, 
             ScopedTypeVariables, FlexibleContexts, FlexibleInstances, 
             OverloadedStrings #-}
module Amelie.DB where

import           Control.Applicative            ((<$>))
import           Control.Monad.State            (MonadState)
import           Control.Monad.State            (gets,modify)
import           Control.Monad.Trans            (MonadIO)
import           Control.Monad.Trans            (liftIO)
import           Data.Char                      (toLower)
import           Data.Data                      (Typeable)
import           Data.List                      (find,nub,intercalate)
import           Data.Maybe                     (listToMaybe)

import           Data.List.Split                (splitWhen)
import           Data.Time.Instances            ()
import           Database.PostgreSQL.Enumerator (DBM,Session,IsolationLevel(..))
import qualified Database.PostgreSQL.Enumerator as DB

import           Amelie.Types                   (Paste(..),Language(..),Channel(..),
                                                 ChansAndLangs,State(..),DBInfo)
import           Amelie.Utils                   (replaceUnless)

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
