{-# LANGUAGE NamedFieldPuns #-}
module Amelie.Links where

import Data.Char    (toLower)
import Data.List    (intercalate,sortBy)
import Data.Maybe   (isJust)
import Data.Ord     (comparing)

import Amelie.Types (PageName)
import Amelie.Utils (replaceUnless)

-- | Link to another page with parameters.
link :: PageName -> [(String,String)] -> String
link name params
  | rewritable name = maybe "" (\f -> f name params) $ lookup name rules
  | otherwise = rewriteBasic name params

rewriteBasic :: PageName -> [(String,String)] -> String
rewriteBasic name = slashParts . (name :) . map spec where
    spec (key,value) = key ++ "/" ++ norm value

-- | Is a page's URL rewritable?
rewritable :: PageName -> Bool
rewritable = isJust . flip lookup rules

-- | Rewrite rules for outgoing links.
rules :: [(PageName,PageName -> [(String,String)] -> String)]
rules = [("paste",rewritePaste),("raw",rewritePaste)] where
  rewritePaste name params = case sortBy (comparing fst) params of
    [("annotation",aid),("pid",pid'),("title",title)] 
      | name == "paste" -> slashParts [name,pid',norm title] ++ "#p" ++ aid
    [("pid",pid'),("title",title)] 
      | name == "raw"   -> slashParts [name,pid',norm title]
      | name == "paste" -> slashParts [pid',norm title]
    [("pid",pid')]                 
      | name == "raw"   -> slashParts [name,pid']
      | name == "paste" -> slashParts [pid']
    _ -> rewriteBasic name params

-- | Normalize a string.
norm :: String -> String
norm = map toLower . replaceUnless '_' valid where
  valid c = any (==toLower c) $ "_" ++ ['a'..'z'] ++ ['0'..'9']

-- | Join a list of string parts into a slash-separated string.
slashParts :: [String] -> String
slashParts = ('/':) . intercalate "/"
