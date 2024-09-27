module MiniGames (main) where

main :: IO ()
main = do
  putStrLn "Hi!  Welcome to Haskell!  It's a fun programming language."
  putStrLn ""
  putStrLn "Here are the results of all the mini games:"
  putStrLn ""
  mapM_ (\(t1, s1, t2, s2) -> putStrLn $ show t1 <> ": " <> show s1 <> "    " <> show t2 <> ": " <> show s2) allGames
  putStrLn ""
  putStrLn "Scoring ratios (points scored / total points played) for all players:"
  putStrLn ""
  flip mapM_ allPlayersScoringRatios $ \(player, pointsScored, pointsPlayed) ->
    putStrLn $
      show player
        <> ": "
        <> show pointsScored
        <> " / "
        <> show pointsPlayed
        <> " = "
        <> show ((fromIntegral pointsScored :: Double) / fromIntegral pointsPlayed)

-- Players participating in the minigame tournament.
data Player
  = Anja
  | Autumn
  | Clea
  | Etta
  | Gwyn
  | Haruka
  | Laine
  | Lauren
  | Layla
  | Morgan
  | Nyah
  | Olivia
  | Story
  deriving (Show, Enum, Eq)

-- A list of all the players.
allPlayers :: [Player]
allPlayers = [Anja .. Story]

-- A team is a list of players.
type Team = [Player]

-- A game is two teams and a score.
type Game = (Team, Int, Team, Int)

-- Each round was 3 games between 3 teams.
round1 :: [Game]
round1 =
  -- The 3 games in round 1.
  [ (team1, 17, team2, 5),
    (team3, 7, team2, 8),
    (team3, 2, team1, 6)
  ]
  where
    -- The 3 teams in round 1.
    team1 = [Etta, Lauren, Olivia, Anja]
    team2 = [Clea, Haruka, Autumn, Gwyn]
    team3 = [Nyah, Morgan, Layla, Laine]

round2 :: [Game]
round2 =
  [ (team2, 4, team3, 6),
    (team2, 5, team1, 7),
    (team3, 7, team1, 3)
  ]
  where
    team1 = [Layla, Morgan, Autumn, Clea]
    team2 = [Gwyn, Olivia, Etta, Laine]
    team3 = [Anja, Lauren, Nyah, Haruka]

round3 :: [Game]
round3 =
  [ (team1, 2, team2, 6),
    (team1, 5, team3, 3),
    (team3, 8, team2, 6)
  ]
  where
    team1 = [Layla, Clea, Story, Nyah, Anja]
    team2 = [Morgan, Haruka, Gwyn, Etta]
    team3 = [Laine, Lauren, Autumn, Olivia]

round4 :: [Game]
round4 =
  [ (team1, 4, team2, 4),
    (team1, 5, team3, 4),
    (team2, 8, team3, 5)
  ]
  where
    team1 = [Laine, Nyah, Layla, Olivia, Gwyn]
    team2 = [Morgan, Clea, Etta, Autumn]
    team3 = [Lauren, Anja, Haruka, Story]

round5 :: [Game]
round5 =
  [ (team1, 8, team2, 8),
    (team1, 6, team3, 4),
    (team2, 8, team3, 3)
  ]
  where
    team1 = [Autumn, Clea, Olivia, Anja, Morgan]
    team2 = [Laine, Lauren, Story, Layla]
    team3 = [Etta, Nyah, Haruka, Gwyn]

-- All the games in on list.
allGames :: [Game]
allGames = round1 <> round2 <> round3 <> round4 <> round5

-- Convert a game into scoring ratios for the two teams.
teamScoringRatios :: Game -> [(Team, Int, Int)]
teamScoringRatios (t1, s1, t2, s2) = [(t1, s1, s1 + s2), (t2, s2, s1 + s2)]

-- Convert all the games into a list of team scoring ratios.
allTeamScoringRatios :: [(Team, Int, Int)]
allTeamScoringRatios = concatMap teamScoringRatios allGames

-- Scoring ratio for a player.
playerScoringRatio :: Player -> (Player, Int, Int)
playerScoringRatio player = (player, sum pS, sum pP)
  where
    (pS, pP) = unzip [(pointsScored, pointsPlayed) | (team, pointsScored, pointsPlayed) <- allTeamScoringRatios, elem player team]

allPlayersScoringRatios :: [(Player, Int, Int)]
allPlayersScoringRatios = playerScoringRatio <$> allPlayers
