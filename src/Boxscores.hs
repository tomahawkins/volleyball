module Boxscores (boxscores) where

import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup

import Match
import PageCache
import Tables
import VolleyParse

-- | Fetch and parse all the boxscores on a team's schedule.
boxscores :: Bool -> String -> IO Season
boxscores refetch url = do
  if refetch then deletePage url else return ()
  putStrLn $ "-- Getting schedule: " ++ url
  a <- getPage url >>= return . parseTags
  m <- mapM match [ f path | TagOpen "a" [("href", path)] <- a, isPrefixOf "boxscore.aspx" path || isPrefixOf "/boxscore.aspx" path ] >>= return . catMaybes
  return $ Season (team m) m
  where
  f a = take 10 url ++ takeWhile (/= '/') (drop 10 url) ++ "/" ++ a

match :: String -> IO (Maybe Match)
match url = do
  putStrLn $ "-- Getting match:    " ++ url
  m@(Match _ sets) <- getPage url >>= return . parseMatch . parseTags
  if null sets then return Nothing else return $ Just m

parseMatch :: [Tag String] -> Match
parseMatch a
  = Match (date a)
  . map parseSet
  . mapMaybe playByPlay
  . tables
  . filter (not . isWhitespace)
  $ a
  where
  date :: [Tag String] -> String
  date a = case a of
    TagOpen "dt" [] : TagText "Date:" : TagClose "dt" : TagText _ : TagOpen "dd" [] : TagText a : _ -> a
    _ : rest -> date rest
    [] -> error "Match date not found."

  isWhitespace :: Tag String -> Bool
  isWhitespace a = case a of
    TagText a -> all isSpace a
    _ -> False

  playByPlay :: Table -> Maybe [[String]]
  playByPlay (p, t) = case p of
    [("class", "sidearm-table play-by-play")] -> Just $ map (map text) t
    _ -> Nothing
    where
    text :: [Tag String] -> String
    text a = case a of
      TagText a : _ -> a
      _ -> ""

parseSet :: [[String]] -> Set
parseSet a = case a of
  [] -> error "No sets in match."
  a : b -> Set $ parseEvents (team' $ index "parseSet" a 4, team' $ index "parseSet" a 8) (0, 0) b

-- Fixes the problem of Somtimes sub team is CLAR, sometimes CLARION.
team' :: String -> String
team' a = case a of
  "CLARION" -> "CLAR"  
  a -> a

index :: Show a => String -> [a] -> Int -> a
index msg a b
  | b < length a = a !! b
  | otherwise = error $ "Index (" ++ show b ++ ") out of range (" ++ msg  ++ "):  " ++ show a

parseEvents :: (Team, Team) -> (Int, Int) -> [[String]] -> [Event]
parseEvents (vTeam, hTeam) (vScore, hScore) a = case a of
  [] -> []
  a : rest
    | null textFull       -> Unknown ""             : rest'
    | isTimeout           -> Timeout                : rest'
    | isSub               -> Sub subTeam subPlayers : rest'
    | servingTeam == "--" -> Unknown (show a) : rest'
    | otherwise           -> Volley servingTeam servingPlayer scoringTeam volley' : rest''
    where
    servingTeam = team' $ a !! 0
    scoringTeam = if vScore == vScore' then hTeam else vTeam
    vScore'     = read $ index "vScore" a 5
    hScore'     = read $ index "hScore" a 7
    rest'       = parseEvents (vTeam, hTeam) (vScore, hScore)   rest
    rest''      = parseEvents (vTeam, hTeam) (vScore', hScore') rest
    textFull    = a !! 3
    isSub       = isInfixOf "subs:" textFull
    isTimeout   = isPrefixOf "Timeout" textFull
    subTeam     = team' $ head $ words textFull
    subPlayers  = map formatPlayer . splitSemi . init . drop 2 . dropUntil ':' $ textFull
    (servingPlayer, volley') = volley . words . concatMap f $ textFull
    formatPlayer = unwords . words . filter (flip notElem ",.")
    f a
      | elem a ",."    = " "
      | elem a "[]();" = " " ++ [a] ++ " "
      | otherwise      = [a]

{-
subGroups :: String -> [a] -> [(a, a)]
subGroups m a = case a of
  [] -> []
  a : b : rest -> (a, b) : subGroups m rest
  [_] -> error $ "subGroups: " ++ m
  -}

splitSemi :: String -> [String]
splitSemi a = case n of
  "" -> []
  n -> n : splitSemi (drop 2 rest)
  where
  (n, rest) = span (/= ';') a

--takeUntil :: Char -> String -> String
--takeUntil a b = takeWhile (/= a) b

dropUntil :: Char -> String -> String
dropUntil a b = dropWhile (/= a) b


