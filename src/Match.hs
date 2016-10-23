module Match
  ( Season (..)
  , Match  (..)
  , Set    (..)
  , Event  (..)
  , Volley (..)
  , Team
  , Name
  , teams
  , team
  , points
  , parseSeasons
  , teamPlayersAll
  , teamPlayersVolley
  ) where

import Data.List
import Data.Maybe

type Name   = String
type Team   = String
data Season = Season Team [Match]
data Match  = Match String [Set]    -- ^ Date and a list of sets.
data Set    = Set   [Event]
data Event
  = Timeout
  | Sub     Team [Name] [Name]              -- ^ Players going in, players going out.
  | Volley  Team (Maybe Name) Team Volley   -- ^ Serving team, serving player, winning team, volley info.
  | Unknown String
  deriving (Show, Read)

data Volley
  = KillBy            (Maybe Name) (Maybe Name) (Maybe Name)   -- ^ Scoring player, setting player, blocking player.
  | AttackError       (Maybe Name) [Name]                      -- ^ Error player, blocking players.
  | ServiceError
  | ServiceAce        (Maybe Name)                             -- ^ Receiving player.
  | BallHandlingError (Maybe Name)                             -- ^ Erroring player.
  | BadSet            (Maybe Name)                             -- ^ Setting player.
  | PointAwarded                                               -- ^ Point awarded for unknown reason.
  deriving (Show, Read, Eq)

instance Show Season where show (Season team matches)  = "Season " ++ team ++ "\n" ++ concatMap show matches
instance Show Match  where show (Match  date sets)  = "Match " ++ date ++ "\n" ++ concatMap show sets
instance Show Set    where show (Set volleys) = "Set\n" ++ concatMap ((++ "\n") . show) volleys

parseSeasons :: String -> [Season]
parseSeasons = f0 . filter (not . isPrefixOf "--") . lines
  where
  mark :: [String] -> String -> Bool
  mark a b = not $ any (flip isPrefixOf b) a

  f0 :: [String] -> [Season]
  f0 a = case a of
    [] -> []
    a : rest
      | isPrefixOf "Season" a -> Season (words a !! 1) (f1 (takeWhile (mark ["Season"]) rest)) : 
                                 f0                        (dropWhile (mark ["Season"]) rest)
      | otherwise -> error "Parse error on parseSeasons.f0."

  f1 :: [String] -> [Match]
  f1 a = case a of
    [] -> []
    a : rest
      | isPrefixOf "Match" a -> Match (words a !! 1) (f2 (takeWhile (mark ["Season", "Match"]) rest)) : 
                                f1                       (dropWhile (mark ["Season", "Match"]) rest)
      | otherwise -> error "Parse error on parseSeasons.f1."

  f2 :: [String] -> [Set]
  f2 a = case a of
    [] -> []
    a : rest
      | isPrefixOf "Set" a -> Set (f3 (takeWhile (mark ["Season", "Match", "Set"]) rest)) : 
                              f2      (dropWhile (mark ["Season", "Match", "Set"]) rest)
      | otherwise -> error "Parse error on parseSeasons.f2."

  f3 :: [String] -> [Event]
  f3 = map read

-- | Teams competing in a match.
teams :: Match -> (Team, Team)
teams m@(Match _ sets) = case nub $ concatMap points sets of
  a : b : _ -> (a, b)
  _         -> error $ "Error infering teams with match:\n" ++ show m

-- | Points of a set.
points :: Set -> [Team]
points (Set a) = mapMaybe f a
  where
  f :: Event -> Maybe Team
  f a = case a of
    Volley _ _ a _ -> Just a
    _ -> Nothing

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

teamPlayersAll :: Team -> Set -> [Name]
teamPlayersAll team (Set events) = nub $ concatMap f events
  where
  f a = case a of
    Timeout -> []
    Unknown _ -> []
    Sub t a b
      | t == team -> a ++ b
      | otherwise -> []
    Volley st sp wt v -> teamPlayersVolley team st sp wt v

--teamPlayersSubs :: Team -> Set -> [Name]
--teamPlayersSubs team (Set events) = nub $ concat [ a | Sub t a <- events, t == team ]
      
teamPlayersVolley :: Team -> Team -> Maybe Name -> Team -> Volley -> [Name]  -- Team of interest, serving team, serving player, winning team, volley.
teamPlayersVolley team st sp wt a = (if st == team then maybeToList sp else []) ++ case a of
  PointAwarded -> []
  KillBy a b c
    | wt == team -> catMaybes [a, b]
    | otherwise  -> maybeToList c
  AttackError a b
    | wt == team -> b
    | otherwise  -> maybeToList a
  ServiceError -> []
  ServiceAce a
    | wt == team -> []
    | otherwise  -> maybeToList a
  BallHandlingError a
    | wt == team -> []
    | otherwise  -> maybeToList a
  BadSet a
    | wt == team -> []
    | otherwise  -> maybeToList a


