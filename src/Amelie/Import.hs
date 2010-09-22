module Main where

import Amelie.DB           (withDB,chansAndLangs,createPaste)
import Amelie.Types        hiding (Tag)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Time
import Safe
import System.Directory
import System.Locale
import System.Time
import Text.HTML.TagSoup
import Text.Parsec         hiding ((<|>))

main :: IO ()
main = do
  st <- newChan
  ch <- newChan 
  writeList2Chan ch [1..28000]
  forM_ ([1..4]::[Int]) $ \_ -> imp st ch
  inserted <- getChanContents st
  mapM_ (\i -> putStrLn $ "Inserting " ++ show i ++ " ...") inserted
  where imp st ch = do
          _ <- forkIO $ forever $ do
              n <- readChan ch
              writeChan st n
              insertPasteFromFile n
          return ()

insertPasteFromFile :: Int -> IO ()
insertPasteFromFile n = do
  p <- parsePasteFile n
  case p of
    Nothing -> return ()
    Just paste -> withDB ("localhost","amelie","amelie") $ do
                    _ <- createPaste paste Nothing
                    return ()

parsePasteFile :: Int -> IO (Maybe Paste)
parsePasteFile pid' = do
  let p = show pid' ++ ".info"
  mt <- getModificationTime p
  ct <- getClockTime
  let delta = tdSec $ diffClockTimes ct mt
  cl <- withDB ("localhost","amelie","amelie") chansAndLangs
  contents <- readFile $ show pid'  ++ ".content"
  let setContents paste = paste { content = replace "\r\n" "\n" contents }
  (fmap setContents . parsePaste ct cl pid' delta) <$> readFile p
  
-- | Parse a paste page into a paste object.
parsePaste :: ClockTime -> ChansAndLangs -> Int -> Int -> String -> Maybe Paste
parsePaste ct (chs,langs) pid' delta = makePaste . parseTags where
  makePaste ts = do
    author' <- get ts "author"
    age <- do ageStr <- parseTimestamp <$> get ts "age"
              case ageStr of
                Right getSeconds -> getSeconds
                Left{}           -> mzero
    language' <- lcase <$> get ts "language"
    title' <- h2 ts
    let td = TimeDiff 0 0 0 0 0 (-(age + delta)) 0
        created' = addToClockTime td ct
        lang' = find (\Language{langName=l} -> language' == lcase l) langs
    return Paste { pid           = pid'
                 , title         = title'
                 , language      = lang'
                 , content       = ""
                 , created       = Just $ calendarToUTC $ toUTCTime created'
                 , author        = author'
                 , expire        = False
                 , tags          = []
                 , annotation_of = Nothing -- Can't be bothered doing annotations.
                 , channel       = find (\Channel{chanName=c'}->language'==c') chs
                 }
  lcase = map toLower

-- | Get the h2.
h2 :: [Tag String] -> Maybe String
h2 (TagOpen "h2" []:TagText t:_) = Just t
h2 []     = Nothing
h2 (_:xs) = h2 xs

-- | Get a value from a key-value element pairing e.g. dt/dd.
get :: [Tag String] -> String -> Maybe String
get ts name = go ts where
  go (TagOpen{}:TagText key:TagClose{}:TagOpen{}:TagText value:xs)
    | key == name = Just value
    | otherwise   = go xs
  go (_tag:xs)    = go xs
  go []           = Nothing

-- | Parse a hpaste2 timestamp into number of seconds.
parseTimestamp :: String -> Either ParseError (Maybe Int)
parseTimestamp = parse timestamp "" where
  timestamp = choice $ map (try . denom) denoms
  denom (n,name) = fmap (*n) . readMay <$> many1 digit <* string (' ' : name)
  denoms = [(60,"minutes"),(3600,"hours"),(3600*24,"days")]

calendarToUTC :: CalendarTime -> UTCTime
calendarToUTC = parseUTCTime . showCalendarTime

parseUTCTime :: String -> UTCTime
parseUTCTime t = fromMaybe err $ parseTime defaultTimeLocale "%Y-%m-%d %T" t
  where err = error $ "Unable to parse " ++ show t ++ ", " ++
                "this shouldn't really happen."

showCalendarTime :: CalendarTime -> String
showCalendarTime = formatCalendarTime defaultTimeLocale "%Y-%m-%d %T"
