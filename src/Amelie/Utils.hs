module Amelie.Utils where

import qualified Data.ByteString.Char8    as B (ByteString)
import qualified Data.ByteString.Char8    as B (concat)
import qualified Data.ByteString.Lazy     as L (ByteString)
import qualified Data.ByteString.Lazy     as L (toChunks)
import           Data.Text                (pack)
import qualified Text.Blaze.Html5         as H
import           Text.Blaze.Renderer.Utf8 (renderHtml)
import           Codec.Binary.UTF8.String   (decodeString,encodeString)

-- | Replace elements of a list with a value, if a predicate is satisfied.
replaceUnless :: a -> (a -> Bool) -> [a] -> [a]
replaceUnless with p = map (\a -> if not (p a) then with else a)

-- | Convert a lazy bytestring to a strict bytestring.
l2s :: L.ByteString -> B.ByteString
l2s  = B.concat . L.toChunks

-- | Make a HTML text object.
text :: String -> H.Html
text = H.text . pack

-- | Clean and encode some text to a UTF8 HTML encoded ByteString.
sanitize :: String -> B.ByteString
sanitize = l2s . renderHtml . text . encodeString