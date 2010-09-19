{-# LANGUAGE NamedFieldPuns #-}
module Amelie.Config where


import Control.Applicative ((<$>))
import System.Directory    (doesFileExist)

import Data.ConfigFile     hiding (content)

import Amelie.Types        (Config(..))

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
  [pastesDir] <- mapM (get c "DIRECTORIES") ["pastesdir"]
  return Config { dbconn        = (host,user,pass) 
                , siteTitle     = title
                , pastesPerPage = read perpage
                , defaultPage   = def
                , templateDir   = temps
                , analytics     = analytics
                , pastesDir     = pastesDir
                }
