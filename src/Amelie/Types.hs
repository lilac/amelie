{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Amelie.Types where

import Control.Monad.State            (MonadState,StateT)
import Control.Monad.Trans            (MonadIO)
import Control.Monad.Trans            (lift)
import Data.Data                      (Data)
import Data.Time                      (UTCTime)
import Data.Typeable                  (Typeable)

import Data.Time.Instances            ()
import Database.PostgreSQL.Enumerator (Session,ConnectA)
import Network.CGI                    (CGIT)
import Network.CGI.Monad              (MonadCGI(..))

-- | A paste.
data Paste =
  Paste { pid           :: Int            -- ^ Database entity id.
        , title         :: Title          -- ^ Title of the paste 
                                          -- ^ (limited to 512~).
        , author        :: Author         -- ^ Author(s) of the paste.
        , language      :: Maybe Language -- ^ Language (if any) of the paste.
        , channel       :: Maybe Channel  -- ^ Associated IRC channel (if any).
        , content       :: Content        -- ^ Raw content of the paste.
        , expire        :: Bool           -- ^ Expires one hour after created?
        , tags          :: [Tag]          -- ^ Tags/keywords for the paste.
        , created       :: Maybe UTCTime  -- ^ When the paste was created.
        , annotation_of :: Maybe Int      -- ^ Paste which this annotates.
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
           } deriving (Typeable,Data,Read,Show,Eq)

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
         , pastesDir     :: FilePath -- ^ Directory for sometimes saving paste files.
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
