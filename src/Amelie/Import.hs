import Control.Monad
import Control.Applicative
import Safe
import Amelie.Types (Paste(..))
import Text.Parsec hiding ((<|>))
import Text.HTML.TagSoup (parseTags,Tag(TagOpen,TagClose,TagText))
import System.Directory
import System.Time

parsePasteFile :: Int -> IO (Maybe Paste)
parsePasteFile pid' = do
  let p = show pid' ++ ".info"
  mt <- getModificationTime p
  ct <- getClockTime
  let delta = tdSec $ diffClockTimes ct mt
  parsePaste pid' delta <$> readFile p
  
-- | Parse a paste page into a paste object.
parsePaste :: Int -> Int -> String -> Maybe Paste
parsePaste pid' delta = makePaste . parseTags where
  makePaste ts = do
    author <- get ts "author"
    age <- do ageStr <- parseTimestamp <$> get ts "age"
              case ageStr of
                Right getSeconds -> getSeconds
                Left{}           -> mzero
    language <- get ts "language"
    title <- h2 ts
    let created = age + delta
    return Paste { pid           = pid'
                 , title         = title
                 , language      = Nothing
                 , channel       = Nothing
                 , content       = ""
                 , created       = Just created
                 , tags          = []
                 , annotation_of = Nothing
                 , author        = author
                 }

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
