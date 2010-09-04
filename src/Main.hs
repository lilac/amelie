module Main where

import qualified Network.CGI     as CGI
import           Network.FastCGI (runFastCGIorCGI)

import           Amelie.Config   (readConfigFile)
import           Amelie.Routes   (runPage,router)

-- | Main entry point.
main :: IO ()
main = do
  result <- readConfigFile "amelie.conf"
  case result of
    Left cperr   -> error $ show cperr
    Right config' -> runFastCGIorCGI $ CGI.handleErrors $ runPage config' router
