{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
module Amelie.Templates where

import           Control.Applicative      ((<$>),(<*>))
import           Control.Monad.State      (MonadState)
import           Control.Monad.State      (gets)
import           Control.Monad.Trans      (MonadIO)
import           Control.Monad.Trans      (liftIO)
import qualified Data.ByteString.Char8    as B (ByteString)
import qualified Data.ByteString.Char8    as B (readFile,pack)
import qualified Data.ByteString.Lazy     as L (ByteString)
import qualified Data.ByteString.Lazy     as L (fromChunks)
import           Data.Char                (toUpper)
import           Data.List                (foldl')
import           System.Directory         (doesFileExist)

import           Codec.Binary.UTF8.String (encodeString)
import           Data.ByteString.Search   (replace)
import           Network.CGI              (CGIResult)
import qualified Network.CGI              as CGI
import           Network.CGI.Monad        (MonadCGI(..))
import           System.FilePath          ((</>))
import qualified Text.Blaze.Html5         as H
import           Text.Blaze.Renderer.Utf8 (renderHtml)

import           Amelie.Pages.Error       (errorPage)
import           Amelie.Types             (State(..),Title,PageName,Config(..))
import           Amelie.Utils             (l2s)

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

-- | Fill a template by a filename and render it to a string.
renderTemplate :: (MonadIO m,MonadState State m)
                  => PageName -> [(String,B.ByteString)] 
                  -> m (Either String L.ByteString)
renderTemplate name ps = do
  tempDir <- gets $ templateDir . config
  let page = (tempDir </> name ++ ".html")
  exists <- liftIO $ doesFileExist page
  if exists
     then do templ <- liftIO $ B.readFile page
             let params = ps
             return $ Right $ fillTemplate params templ
     else return $ Left $ "No such template file: " ++ show (tempDir </> name)

-- | Fill a template's parameters.
fillTemplate :: [(String,B.ByteString)] -> B.ByteString -> L.ByteString
fillTemplate xs str = L.fromChunks . return $ foldl' rep str xs where
  rep acc (this,with) = l2s $ replace this' with acc where
    this' = B.pack $ "<$" ++ map toUpper this ++ "$>"
