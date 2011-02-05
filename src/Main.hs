module Main where

import qualified Network.CGI     as CGI
import           Network.FastCGI (runFastCGIConcurrent)

import           Amelie.Config   (readConfigFile)
-- import           Amelie.Expiry   (runExpiryTask)
import           Amelie.Routes   (runPage,router)

-- | Main entry point.
main :: IO ()
main = do
  result <- readConfigFile "amelie.conf"
  case result of
    Left cperr   -> error $ show cperr
    Right config' -> do
--      runExpiryTask config'
      runFastCGIConcurrent 1000 $ CGI.handleErrors $ runPage config' router
