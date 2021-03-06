module Amelie.Expiry where

import           Control.Monad                  (forever)
import           Control.Concurrent             (forkIO,threadDelay)
import Data.Time

import qualified Database.PostgreSQL.Enumerator as DB

import           Amelie.Types                   (Config(..))


runExpiryTask :: Config -> IO ()
runExpiryTask Config{dbconn=(host,user,pass)} = do
  _ <- forkIO $ forever $ do
    t <- getCurrentTime
    appendFile "expire.log" $ show t ++ ": Running expiry...\n"
    DB.withSession connector $
      DB.execDDL $ DB.sql "delete from paste where expire < now()"
    threadDelay $ 1000 * 1000 * 30
  return ()
  where connector = DB.connect [DB.CAhost host,DB.CAuser user,DB.CApassword pass]
