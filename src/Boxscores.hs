module Boxscores (boxscores) where

import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup

import Match
import PageCache
import Tables

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
  a : b -> Set $ parseEvents (index "paseSet" a 4, index "parseSet" a 8) (0, 0) b

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
    | otherwise     -> Volley servingTeam scoringTeam (parseVolley textFull) : rest''
    where
    servingTeam = a !! 0
    scoringTeam = if vScore == vScore' then hTeam else vTeam
    vScore'     = read $ index "vScore" a 5
    hScore'     = read $ index "hScore" a 7
    rest'       = parseEvents (vTeam, hTeam) (vScore, hScore)   rest
    rest''      = parseEvents (vTeam, hTeam) (vScore', hScore') rest
    textFull    = a !! 3
    isSub       = isInfixOf "subs:" textFull
    isTimeout   = isPrefixOf "Timeout" textFull
    subTeam     = head $ words textFull
    subPlayers  = splitSemi . init . drop 2 . dropUntil ':' $ textFull

parseVolley :: String -> Volley
parseVolley textFull
  | isKillBy            = KillBy            bracketPlayer killPlayer fromPlayer blockingPlayer
  | isAttackError       = AttackError       bracketPlayer attackErrorPlayer blockingPlayers
  | isServiceError      = ServiceError      bracketPlayer
  | isServiceAce        = ServiceAce        bracketPlayer aceReceivePlayer
  | isBallHandlingError = BallHandlingError bracketPlayer handlingErrorPlayer
  | isBadSet            = BadSet            bracketPlayer badSetPlayer
  | isPointAwarded      = PointAwarded
  | otherwise           = error $ "parseVolley: " ++ show textFull
  where

  -- XXX This doesn't parse:  [Someone] Kill by Someone Else, block error by Somebody.

  -- Text segments.
  textMinusBracket
    | head textFull == '[' = init . drop 2 . dropUntil ']' $ textFull
    | otherwise            = textFull
  textBase
    | elem '(' textMinusBracket = init . takeUntil '(' $ textMinusBracket
    | otherwise                 = textMinusBracket
  textParens
    | elem '(' textMinusBracket = takeUntil ')' . tail . dropUntil '(' $ textMinusBracket
    | otherwise                 = ""
  textPostParens
    | elem '(' textMinusBracket = drop 3 . dropUntil ')' $ textMinusBracket
    | otherwise                 = ""

  bracketPlayer = takeUntil ']' . tail $ textFull

  textHas             = flip isPrefixOf textBase
  isKillBy            = textHas "Kill by"
  isAttackError       = textHas "Attack error by"
  isServiceError      = textHas "Service error"
  isServiceAce        = textHas "Service ace"
  isBadSet            = textHas "Bad set by"
  isBallHandlingError = textHas "Ball handling error by"
  isPointAwarded      = textHas "Point awarded by official to"
  fromPlayer'         = drop  5 textParens
  fromPlayer          = if null fromPlayer' then Nothing else Just fromPlayer'
  killPlayer          = drop  8 textBase
  blockingPlayer
    | isPrefixOf "block error by " textPostParens = Just . drop 15 $ textPostParens
    | otherwise                                   = Nothing
  attackErrorPlayer   = drop 16 textBase
  aceReceivePlayer    = textParens
  blockingPlayers     = splitSemi . drop 9 $ textParens
  handlingErrorPlayer = drop 23 textBase
  badSetPlayer        = drop 11 textBase

splitSemi :: String -> [String]
splitSemi a = case n of
  "" -> []
  n -> n : splitSemi (drop 2 rest)
  where
  (n, rest) = span (/= ';') a

takeUntil :: Char -> String -> String
takeUntil a b = takeWhile (/= a) b

dropUntil :: Char -> String -> String
dropUntil a b = dropWhile (/= a) b


