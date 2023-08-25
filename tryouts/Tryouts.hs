{-# LANGUAGE LambdaCase #-}

module Tryouts
  ( main,
    report,
  )
where

import Data.List (intercalate, intersect, (\\))
import Match (match)
import Players (Player (..), Spot (..), allPlayers)
import RankingsByCoaches (defense, middles, pins, setters)
import RankingsByPlayers (rankingsByPlayers)

main :: IO ()
main = putStrLn report

report :: String
report =
  convertToDos $ unlines $
    [ "2023 BHS Volleyball Tryout Match Report",
      "",
      "Total number of players: " <> show (length allPlayers),
      "",
      "Players interested in outside hitter:",
      "",
      indent $ itemList $ interestedIn [VarsityOutside, JvOutside, CteamOutside],
      "",
      "Players interested in right-side hitter:",
      "",
      indent $ itemList $ interestedIn [VarsityRightside, JvRightside, CteamRightside],
      "",
      "Players interested in middle blocker:",
      "",
      indent $ itemList $ interestedIn [VarsityMiddle, JvMiddle, CteamMiddle],
      "",
      "Players interested in defensive specialist, libero:",
      "",
      indent $ itemList $ interestedIn [VarsityDefense, JvDefense, CteamDefense],
      "",
      "Players interested in setter:",
      "",
      indent $ itemList $ interestedIn [VarsitySetter, JvSetter, CteamSetter],
      "",
      "Players and their placement rankings:",
      "",
      intercalate "\n" ["  " <> show p <> ":\n" <> formatSpots spots | (p, spots) <- rankingsByPlayers'],
      "",
      "Coaches rankings of players at positions:",
      "",
      "  Outside and right-side hitters:",
      "",
      indent $ indent $ numberList pins,
      "  Middle blockers:",
      "",
      indent $ indent $ numberList middles,
      "  Defensive specialists and liberos:",
      "",
      indent $ indent $ numberList defense,
      "  Setters:",
      "",
      indent $ indent $ numberList setters,
      "",
      "Match results:",
      "",
      unlines (formatMatch <$> matches),
      unlines ["  DevTeam: " <> show p | p <- unplacedPlayers]
    ]

convertToDos :: String -> String
convertToDos = concatMap $ \case
  '\n' -> "\r\n"
  a -> [a]

numberList :: Show a => [a] -> String
numberList a = unlines [show n <> ". " <> show item | (n, item) <- zip [1 :: Int ..] a]

itemList :: Show a => [a] -> String
itemList a = unlines $ (("- " <>) . show) <$> a

indent :: String -> String
indent s = unlines $ ("  " <>) <$> lines s

formatSpots :: [Spot] -> String
formatSpots = \case
  [] -> "    ???\n"
  spots -> indent $ indent $ numberList spots

formatMatch :: (Spot, Maybe Player) -> String
formatMatch = \case
  (s, Nothing) -> "  " <> show s <> ": ____"
  (s, Just p') -> "  " <> show s <> ": " <> show p'

interestedIn :: [Spot] -> [Player]
interestedIn pos = [p | p <- allPlayers, not $ null $ intersect pos $ rankingsByPlayers p]

matches :: [(Spot, Maybe Player)]
matches = match rankingsBySpots rankingsByPlayers'

unplacedPlayers :: [Player]
unplacedPlayers = allPlayers \\ [p | (_, Just p) <- matches]

rankingsByPlayers' :: [(Player, [Spot])]
rankingsByPlayers' = [(p, rankingsByPlayers p) | p <- allPlayers]

rankingsBySpots :: [(Spot, [Player])]
rankingsBySpots =
  rep 1 VarsitySetter setters
    <> rep 2 VarsityOutside pins
    <> rep 1 VarsityRightside pins
    <> rep 2 VarsityMiddle middles
    <> rep 2 VarsityDefense defense
    <> rep 2 JvSetter setters
    <> rep 3 JvOutside pins
    <> rep 2 JvRightside pins
    <> rep 3 JvMiddle middles
    <> rep 2 JvDefense defense
    <> rep 2 CteamSetter setters
    <> rep 3 CteamOutside pins
    <> rep 2 CteamRightside pins
    <> rep 3 CteamMiddle middles
    <> rep 2 CteamDefense defense
  where
    rep n s p = replicate n (s, p)
