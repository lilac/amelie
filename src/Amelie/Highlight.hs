{-# LANGUAGE NamedFieldPuns #-}
module Amelie.Highlight where

import           Control.Monad          (mplus)
import           Data.Char              (toLower)
import           Data.Maybe             (fromMaybe)

import           Text.Highlighting.Kate (FormatOption(..))
import qualified Text.Highlighting.Kate as Kate

import           Amelie.Types           (Paste(..),Language(..))

-- | Highlighting CSS.
highlightCSS :: String
highlightCSS = Kate.defaultHighlightingCss

-- | Syntax highlight a paste.
pasteHighlightedHtml :: Paste -> Maybe Language -> Maybe String
pasteHighlightedHtml Paste{content,language} lang =
  let langName' = fromMaybe "" $ fmap langName $ lang `mplus` language
  in case Kate.highlightAs (map toLower langName') content of
       Left _err -> Nothing
       Right slines -> Just $ show $
         Kate.formatAsXHtml [OptNumberLines,OptLineAnchors] langName' slines

