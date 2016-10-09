module Match
  ( Match
  , Set
  , Volley (..)
  , matches
  , match
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup

import PageCache
import Tables

matches :: String -> IO [Match]
matches url = do
  putStrLn $ "Getting schedule: " ++ url
  a <- getPage url >>= return . parseTags
  mapM match [ f path | TagOpen "a" [("href", path)] <- a, isPrefixOf "boxscore.aspx" path ]
  where
  f a = take 10 url ++ takeWhile (/= '/') (drop 10 url) ++ "/" ++ a

match :: String -> IO Match
match url = do
  putStrLn $ "Getting match: " ++ url
  getPage url >>= return . parseMatch

parseMatch :: String -> Match
parseMatch
  = map parseSet
  . mapMaybe playByPlay
  . tables
  . filter (not . isWhitespace)
  . parseTags
  where
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
  _ : m-> map parseVolley m

parseVolley :: [String] -> Volley
parseVolley a
  | length a < 4 = Unknown $ show a
  | isTimeout = Timeout
  | isSub     = Sub subTeam subPlayers
  | isKillBy  = KillBy pointTeam bracketPlayer killPlayer fromPlayer
  | isAttackError  = AttackError pointTeam bracketPlayer attackErrorPlayer blockingPlayers
  | isServiceError = ServiceError pointTeam bracketPlayer
  | isServiceAce   = ServiceAce pointTeam bracketPlayer aceReceivePlayer
  | isBallHandlingError = BallHandlingError pointTeam bracketPlayer handlingErrorPlayer
  | isBadSet            = BadSet pointTeam bracketPlayer badSetPlayer
  | otherwise = Unknown text
  where
  pointTeam = a !! 0
  text      = a !! 3
  subTeam   = head $ words text
  subPlayers = splitSemi . init . unwords . drop 2 . words $ text
  textHas = flip isInfixOf text
  isSub          = textHas "subs:"
  isTimeout      = textHas "Timeout"
  isKillBy       = textHas "Kill by"
  isAttackError  = textHas "Attack error by"
  isServiceError = textHas "Service error"
  isServiceAce   = textHas "Service ace"
  isBadSet       = textHas "Bad set by"
  isBallHandlingError = textHas "Ball handling error by"
  bracketPlayer = takeWhile (/= ']') . tail $ text
  action        = init . drop 2 . dropWhile (/= ']') $ text
  parenText
    | elem '(' action = takeWhile (/= ')') . tail . dropWhile (/= '(') $ action
    | otherwise       = ""
  fromPlayer    = drop 5 parenText
  killPlayer    = takeWhile (/= '(') . drop 8 $ action
  attackErrorPlayer = takeWhile (/= '(') . drop 16 $ action
  aceReceivePlayer = parenText
  blockingPlayers = splitSemi . drop 9 $ parenText
  handlingErrorPlayer = drop 23 $ action
  badSetPlayer = drop 11 $ action

splitSemi :: String -> [String]
splitSemi a = case n of
  "" -> []
  n -> n : splitSemi (drop 2 rest)
  where
  (n, rest) = span (/= ';') a

type Name  = String
type Team  = String
type Match = [Set]
type Set   = [Volley]

data Volley
  = Timeout
  | Sub               Team [Name]           -- ^ Players going in.
  | KillBy            Team Name Name Name   -- ^ Team, bracket player, scoring player, assisting player.
  | AttackError       Team Name Name [Name] -- ^ Team, bracket player, error player, blocking players.
  | ServiceError      Team Name             -- ^ Team, bracket player.
  | ServiceAce        Team Name Name        -- ^ Team, bracket player (serving), receiving player.
  | BallHandlingError Team Name Name        -- ^ Team, bracket player, erroring player.
  | BadSet            Team Name Name        -- ^ Team, bracket player, setting player.
  | Unknown           String
  deriving Show

