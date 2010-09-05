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
rules = [("paste",rewritePaste),("raw",rewritePaste),("control",rewritePaste)] where
  rewritePaste name params = case sortBy (comparing fst) params of
    [("pid",pid'),("title",title)]
      | name == "control" -> rewriteBasic name [("pid",pid'),("title",flat title)]
    [("annotation",aid),("pid",pid'),("title",title)] 
      | name == "paste" -> slashParts [name,pid',flat title] ++ "#p" ++ aid
    [("pid",pid'),("title",title)] 
      | name == "raw"   -> slashParts [name,pid',flat title]
      | name == "paste" -> slashParts [pid',flat title]
    [("pid",pid')]                 
      | name == "raw"   -> slashParts [name,pid']
      | name == "paste" -> slashParts [pid']
    _ -> rewriteBasic name params

-- | Flatten a string.
flat :: String -> String
flat = trim '_' . norm . take 30

-- | Trim a string on either end.
trim :: Char -> String -> String
trim c = dropWhile (==c) . reverse . dropWhile (==c) . reverse

-- | Normalize a string.
norm :: String -> String
norm = map toLower . replaceUnless '_' valid . filter validOrSpace where
  valid c = any (==toLower c) $ "_" ++ ['a'..'z'] ++ ['0'..'9']
  validOrSpace c = valid c || c == ' '

-- | Join a list of string parts into a slash-separated string.
slashParts :: [String] -> String
slashParts = ('/':) . intercalate "/"
