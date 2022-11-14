{-# LANGUAGE OverloadedStrings #-}


-- | Caching pages, schedules, rosters, play-by-plays, etc.
module Groq.Free.Ball.Cache
  ( Url
  , ScheduleUrl
  , BoxscoreUrl
  , Team
  , Player
  , hashUrl
  , getPage
  , getBoxscoreUrls
  , getPlays
  , getAssists
  ) where


import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Digest.Pure.SHA           ( sha1
                                                , showDigest
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                )
import           System.Process                 ( readProcess )
import           Text.HTML.TagSoup              ( Tag(..)
                                                , parseTags
                                                , renderTags
                                                )


type Url = String
type ScheduleUrl = Url
type BoxscoreUrl = Url
type Team = Text
type Player = Text


-- | Generic caching mechanism.
getCache
  :: (FilePath, String)  -- ^ Directory and file extension.
  -> (a -> String)       -- ^ Serialize.
  -> (String -> a)       -- ^ Unserialize.
  -> (Url -> IO a)       -- ^ Fetch.
  -> Url                 -- ^ Url source.
  -> IO a
getCache (dir, ext) serialize unserialize fetch url = do
  f <- doesFileExist file
  if f
    then do
      contents <- readFile file
      pure $ unserialize contents
    else do
      createDirectoryIfMissing True dir'
      dat <- fetch url
      writeFile file $ serialize dat
      pure dat

 where

  dir' = "cache/" <> dir
  file = dir' <> "/" <> hashUrl url <> ext


hashUrl :: String -> String
hashUrl = showDigest . sha1 . B.pack


-- | Get a page from the cache.  Fetch from web if page doesn't exist.
getPage :: Url -> IO [Tag Text]
getPage = getCache
  ("page", ".html")
  (T.unpack . renderTags)
  (parseTags . T.pack)
  (\url -> (parseTags . T.pack) <$> readProcess "curl" [url] "")



-- | Returns the boxscore urls given a schedule url for the season.
getBoxscoreUrls :: ScheduleUrl -> IO [BoxscoreUrl]
getBoxscoreUrls = getCache ("schedule", ".hs")
                           show
                           read
                           (\url -> schedule url <$> getPage url)

 where

  baseUrl url = "https://" <> takeWhile (/= '/') (drop 8 url)
  schedule url page =
    [ baseUrl url <> T.unpack link
    | TagOpen "a" (_ : ("href", link) : _) <- page
    , T.isInfixOf "/boxscore/" link
    ]


-- | Given boxscore url, return plays.
getPlays :: BoxscoreUrl -> IO [[(Team, Player)]]
getPlays =
  getCache ("plays", ".hs") show read (\url -> servesPerSet <$> getPage url)


-- | Serves per set.
servesPerSet :: [Tag Text] -> [[(Team, Player)]]
servesPerSet = \case
  [] -> []
  TagOpen "section" (("id", set) : _) : rest
    | elem set [ "set-" <> showT i | i <- [1 .. 5 :: Int] ] -> names
    : servesPerSet rest'
   where
    (names, rest') =
      extractServesInSet [] $ dropWhile (/= TagOpen "tbody" []) rest
  _ : rest -> servesPerSet rest


extractServesInSet
  :: [(Team, Player)] -> [Tag Text] -> ([(Team, Player)], [Tag Text])
extractServesInSet sofar tags = case tags of

  [] -> error "Unexpected EOF."

  TagOpen "tr" _ : TagText _ : TagOpen "td" _ : TagText team : TagClose "td" : TagText _ : TagOpen "td" _ : TagText _ : TagClose "td" : TagText _ : TagOpen "td" _ : TagClose "td" : TagText _ : TagOpen "td" _ : TagText play : TagClose "td" : TagText _ : rest
    | team /= "--"
    -> extractServesInSet (sofar <> [(team, server)]) rest
    | otherwise
    -> extractServesInSet sofar rest
    where server = T.pack $ takeWhile (/= ']') $ drop 1 $ T.unpack play

  TagClose "tbody" : rest -> (sofar, rest)

  _                : rest -> extractServesInSet sofar rest


showT :: Show a => a -> Text
showT = T.pack . show


-- | Get game leaders for assists.
getAssists :: BoxscoreUrl -> IO [(Player, Int)]
getAssists =
  getCache ("assists", ".hs") show read (\url -> assists <$> getPage url)


assists :: [Tag Text] -> [(Player, Int)]
assists tags = concatMap cleanRow rows

 where

  tables = filterTables tags
  rows   = assistsRows tables

  assistsRows :: [Tag Text] -> [[Tag Text]]
  assistsRows = \case
    [] -> []
    TagOpen "th" _ : TagText "Assists" : TagClose "th" : TagText _ : TagOpen "td" _ : rest
      -> assistsRows' [] rest
    _ : rest -> assistsRows rest

  assistsRows' :: [Tag Text] -> [Tag Text] -> [[Tag Text]]
  assistsRows' sofar a = case a of
    []                   -> error "Expecting </td>."
    TagClose "td" : rest -> sofar : assistsRows rest
    b             : rest -> assistsRows' (sofar <> [b]) rest

  cleanRow :: [Tag Text] -> [(Player, Int)]
  cleanRow row = parseAssistText $ liftText row

  liftText :: [Tag Text] -> Text
  liftText = \case
    []               -> ""
    TagText t : rest -> t <> liftText rest
    TagOpen "a" _ : TagText t : TagClose "a" : rest -> t <> liftText rest
    _         : _    -> error "Unexpected tag (liftText)."


parseAssistText :: Text -> [(Player, Int)]
parseAssistText = \case
  ""   -> []
  rest -> (name, count) : parseAssistText rest'
   where
    (before, after)   = splitWhile (/= ')') rest  -- Split at next ')'.
    rest'             = T.drop 3 after  -- Drop the "), " or just the ")" if at the end.
    (before', after') = splitWhile (/= '(') before  -- Split at the '('.
    name              = T.init before'  -- Drop the trailing " ".
    count             = read $ T.unpack $ T.tail after'  -- Drop the leading "(" and convert to Int.


splitWhile :: (Char -> Bool) -> Text -> (Text, Text)
splitWhile p t = (T.takeWhile p t, T.dropWhile p t)


-- | Get the game leader tables.
filterTables :: [Tag Text] -> [Tag Text]
filterTables = \case
  [] -> []
  t@(TagOpen "table" [("class", "sidearm-table overall-stats centered-caption")]) : rest
    -> t : filterTables' rest
  _ : rest -> filterTables rest


filterTables' :: [Tag Text] -> [Tag Text]
filterTables' = \case
  []                          -> error "Table tag not closed."
  t@(TagClose "table") : rest -> t : filterTables rest
  t                    : rest -> t : filterTables' rest


