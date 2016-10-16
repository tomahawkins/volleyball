module Match
  ( Season (..)
  , Match  (..)
  , Set    (..)
  , Volley (..)
  , Team
  , Name
  , season
  , match
  , teams
  , team
  , points
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup

import PageCache
import Tables

type Name   = String
type Team   = String
data Season = Season Team [Match]
data Match  = Match String [Set]    -- ^ Date and a list of sets.
data Set    = Set   [Volley]

data Volley
  = Timeout
  | Sub               Team [Name]           -- ^ Players going in.
  | KillBy            Team Name Name Name   -- ^ Team, bracket player, scoring player, assisting player.
  | AttackError       Team Name Name [Name] -- ^ Team, bracket player, error player, blocking players.
  | ServiceError      Team Name             -- ^ Team, bracket player.
  | ServiceAce        Team Name Name        -- ^ Team, bracket player (serving), receiving player.
  | BallHandlingError Team Name Name        -- ^ Team, bracket player, erroring player.
  | BadSet            Team Name Name        -- ^ Team, bracket player, setting player.
  | PointAwarded      Team                  -- ^ Team.  Point awarded for unknown reason.
  | Unknown           String
  deriving (Show, Read)

instance Show Season where show (Season team matches)  = "Season " ++ team ++ "\n" ++ concatMap show matches
instance Show Match  where show (Match  date sets)  = "Match " ++ date ++ "\n" ++ concatMap show sets
instance Show Set    where show (Set volleys) = "Set\n" ++ concatMap ((++ "\n") . show) volleys

season :: Bool -> String -> IO Season
season refetch url = do
  if refetch then deletePage url else return ()
  putStrLn $ "Getting schedule: " ++ url
  a <- getPage url >>= return . parseTags
  m <- mapM match [ f path | TagOpen "a" [("href", path)] <- a, isPrefixOf "boxscore.aspx" path || isPrefixOf "/boxscore.aspx" path ] >>= return . catMaybes
  return $ Season (team m) m
  where
  f a = take 10 url ++ takeWhile (/= '/') (drop 10 url) ++ "/" ++ a

match :: String -> IO (Maybe Match)
match url = do
  putStrLn $ "Getting match:    " ++ url
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
  _ : m-> Set $ map parseVolley m

parseVolley :: [String] -> Volley
parseVolley a
  | length a < 4        = Unknown $ show a
  | null textFull       = Unknown ""
  | isTimeout           = Timeout
  | isSub               = Sub subTeam subPlayers
  | isKillBy            = KillBy pointTeam bracketPlayer killPlayer fromPlayer
  | isAttackError       = AttackError pointTeam bracketPlayer attackErrorPlayer blockingPlayers
  | isServiceError      = ServiceError pointTeam bracketPlayer
  | isServiceAce        = ServiceAce pointTeam bracketPlayer aceReceivePlayer
  | isBallHandlingError = BallHandlingError pointTeam bracketPlayer handlingErrorPlayer
  | isBadSet            = BadSet pointTeam bracketPlayer badSetPlayer
  | isPointAwarded      = PointAwarded pointTeam
  | otherwise           = Unknown textFull
  where
  pointTeam = a !! 0

  {-
  init' m a = case a of
    [] -> error $ "init' (" ++ m ++ "): \"" ++ textFull ++ "\""
    a -> init a

  tail' m a = case a of
    [] -> error $ "tail' (" ++ m ++ "): \"" ++ textFull ++ "\""
    a -> tail a

  head' m a = case a of
    [] -> error $ "head' (" ++ m ++ "): \"" ++ textFull ++ "\""
    a -> head a
  -}

  -- Text segments.
  textFull         = a !! 3
  textMinusBracket
    | head textFull == '[' = init . drop 2 . dropWhile (/= ']') $ textFull
    | otherwise            = textFull
  textBase
    | elem '(' textMinusBracket = init . takeWhile (/= '(') $ textMinusBracket
    | otherwise                 = textMinusBracket
  textParens
    | elem '(' textMinusBracket = init . tail . dropWhile (/= '(') $ textMinusBracket
    | otherwise                 = ""

  bracketPlayer = takeWhile (/= ']') . tail $ textFull

  subTeam    = head $ words textFull
  subPlayers = splitSemi . init . drop 2 . dropWhile (/= ':') $ textFull
  textHas             = flip isPrefixOf textBase
  isSub               = isInfixOf "subs:" textFull
  isTimeout           = textHas "Timeout"
  isKillBy            = textHas "Kill by"
  isAttackError       = textHas "Attack error by"
  isServiceError      = textHas "Service error"
  isServiceAce        = textHas "Service ace"
  isBadSet            = textHas "Bad set by"
  isBallHandlingError = textHas "Ball handling error by"
  isPointAwarded      = textHas "Point awarded by official to"
  fromPlayer          = drop  5 textParens
  killPlayer          = drop  8 textBase
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

-- | Teams competing in a match.
teams :: Match -> (Team, Team)
teams m@(Match _ sets)
  | length a == 2 = (a !! 0, a !! 1)
  | otherwise     = error $ "Error infering teams with match:\n" ++ show m
  where
  a = nub $ concatMap points sets

-- | Points of a set.
points :: Set -> [Team]
points (Set a) = mapMaybe f a
  where
  f :: Volley -> Maybe Team
  f a = case a of
    Sub               t _     -> Just t
    KillBy            t _ _ _ -> Just t
    AttackError       t _ _ _ -> Just t
    ServiceError      t _     -> Just t
    ServiceAce        t _ _   -> Just t
    BallHandlingError t _ _   -> Just t
    BadSet            t _ _   -> Just t
    PointAwarded      t       -> Just t
    Timeout                   -> Nothing
    Unknown _                 -> Nothing

-- | Team of a list of matches.
team :: [Match] -> Team
team a = case a of
  m1 : m2 : _ -> case (teams m1, teams m2) of
    ((a, b), (c, d))
      | a == c -> a
      | a == d -> a
      | b == c -> b
      | b == d -> b
      | otherwise -> error "team"
  _ -> error "team"

