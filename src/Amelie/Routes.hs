{-# LANGUAGE NamedFieldPuns #-}
module Amelie.Routes where

import           Control.Applicative            ((<$>))
import           Control.Monad.State            (gets,runStateT)
import           Control.Monad.Trans            (liftIO)
import           Data.List                      (union)

import           Data.List.Split                (splitWhen)
import           Data.Time.Instances            ()
import qualified Database.PostgreSQL.Enumerator as DB
import           Network.CGI                    (CGIResult,CGI)
import qualified Network.CGI                    as CGI

import           Amelie.DB                      (db,chansAndLangs)
import qualified Amelie.Pages                   as Pages
import           Amelie.Types                   (SCGI(..),Config(..),State(..))

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
    "paste" -> cls $ Pages.pastePage params
    "raw"   -> cls $ Pages.rawPastePage params
    "new"   -> cls Pages.newPastePage
    _       -> Pages.pastesPage

-- | Run a sessioned CGI page with a database connection and configuration.
runPage :: Config -> SCGI CGIResult -> CGI CGIResult
runPage config@Config{dbconn=(host,user,pass)} m = do
  ((),sess) <- liftIO $ DB.withContinuedSession connector (return ())
  let state = State { dbsess = sess, config = config }
  (cgiResult,State{dbsess}) <- flip runStateT state $ runSCGI m
  liftIO $ DB.withSession dbsess $ return ()
  return cgiResult
  where connector = DB.connect [DB.CAhost host,DB.CAuser user,DB.CApassword pass]
