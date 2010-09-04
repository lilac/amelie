module Amelie.Pages.Error where

import           Network.CGI       (CGIResult)
import qualified Network.CGI       as CGI
import           Network.CGI.Monad (MonadCGI(..))

-- | A friendly error page.
errorPage :: MonadCGI m => String -> m CGIResult
errorPage = CGI.output
