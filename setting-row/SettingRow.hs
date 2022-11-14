{-# LANGUAGE OverloadedStrings #-}


module Groq.Free.Ball.SettingRow
  ( main
  , withBoxscore
  ) where


import           Control.Monad                  ( forM )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Text.Printf


import           Groq.Free.Ball.Cache           ( Player
                                                , Url
                                                , BoxscoreUrl
                                                , getAssists
                                                , getBoxscoreUrls
                                                , getPlays
                                                )
import qualified Groq.Free.Ball.Cache          as C


-- | Distinguish teams by which team serves first and second.
data Team = First | Second deriving (Show, Eq)


-- | Floor position.  Server at P1.
--
--   -------------- Net ---------------
--   |          |          |          |
--   |    P4    |    P3    |    P2    |
--   |          |          |          |
--   |----------|----------|----------|
--   |          |          |          |
--   |    P5    |    P6    |    P1    |
--   |          |          |          |
--   |----------|----------|----------|
--
data Position = P1 | P2 | P3 | P4 | P5 | P6 deriving (Show, Eq, Ord)


-- | Rotate forward one position.
rotatePosition :: Position -> Position
rotatePosition = \case
  P1 -> P6
  P6 -> P5
  P5 -> P4
  P4 -> P3
  P3 -> P2
  P2 -> P1


-- | Rotate back one position.
rotateBackPosition :: Position -> Position
rotateBackPosition =
  rotatePosition
    . rotatePosition
    . rotatePosition
    . rotatePosition
    . rotatePosition


-- | Front row or back row.
data Row = Front | Back deriving (Show, Eq)


-- | Row given position.
row :: Position -> Row
row = \case
  P1 -> Back
  P2 -> Front
  P3 -> Front
  P4 -> Front
  P5 -> Back
  P6 -> Back


-- | Infer setters from game leader assist stats.
--   Anyone assiting more than 17% of the time is probably a setter.
setters :: [(Player, Int)] -> [Player]
setters stats = [ p | (p, s) <- stats, s >= threshold ]
 where
  totalSets = sum $ snd $ unzip stats
  threshold = totalSets `div` 6  -- 17%


-- | A sequece of serving teams (First/Second) for a game set
--   given a serving sequence of team names.
servingTeams :: [Text] -> [Team]
servingTeams teams = whichTeam <$> teams
 where
  whichTeam t | t == head teams = First
              | otherwise       = Second


-- | From sequence of serving teams, determine who scored each point.
scoringTeams :: [Team] -> [Team]
scoringTeams serving = f (0, 0) serving
 where
  f :: (Int, Int) -> [Team] -> [Team]
  f (firstPoints, secondPoints) teams = case teams of
    []                    -> []
    _ : rest@(First  : _) -> First : f (firstPoints + 1, secondPoints) rest
    _ : rest@(Second : _) -> Second : f (firstPoints, secondPoints + 1) rest
    [_] | firstPoints > secondPoints -> [First]
        | otherwise                  -> [Second]


-- | Determine setter positions for first and second teams for each point.
--   Returns (scoring team, first, and second setter positions).
setterPositions
  :: [Player] -> [(C.Team, Player)] -> [(Team, Set Position, Set Position)]
setterPositions gameSetters servingOrder = zip3 scoringTeams' first second

 where

  servingTeams'  = servingTeams $ fst $ unzip servingOrder
  servingPlayers = snd $ unzip servingOrder
  scoringTeams'  = scoringTeams servingTeams'
  first          = propagatePositions $ zip (sideouts First) (initial First)
  second         = propagatePositions $ zip (sideouts Second) (initial Second)

  initial :: Team -> [Set Position]
  initial team =
    [ if team' == team && elem player gameSetters
        then Set.singleton P1
        else Set.empty
    | (team', player) <- zip servingTeams' servingPlayers
    ]

  sideouts :: Team -> [Bool]
  sideouts team =
    False
      : [ team2 == team && team1 /= team2
        | (team1, team2) <- zip servingTeams' $ tail servingTeams'
        ]


-- | Propagate setting positions for all rotations in a set.
--   The Bool indicates a rotation, i.e. a side out.
propagatePositions :: [(Bool, Set Position)] -> [Set Position]
propagatePositions = \case
  []       -> error "Not expecting [].  (propagatePositions)"
  [(_, p)] -> [p]
  (_, p1) : (rotate, p2) : rest ->
    case propagatePositions $ (rotate, Set.union p1' p2) : rest of
      []          -> error "Not expecting [].  (propagatePositions)"
      p2' : rest' -> Set.union p1 p2'' : p2' : rest'

       where

        p2'' | rotate    = Set.map rotateBackPosition p2'
             | otherwise = p2'

   where

    p1' | rotate    = Set.map rotatePosition p1
        | otherwise = p1


-- | Determine total wins when setter is in (front, back) rows
--   given list of setters and serve sequence for a set.
winningSetterRows :: [Player] -> (Int, [(C.Team, Player)]) -> IO (Int, Int)
winningSetterRows gameSetters (setNumber, servingOrder) = do
  putStrLn $ "Set " <> show setNumber <> ":"
  mapM_ print $ setterPositions gameSetters servingOrder
  putStrLn ""
  putStrLn $ "Results: " <> show (sum front, sum back)
  putStrLn ""
  pure (sum front, sum back)
 where
  (front, back) = unzip
    [ points scoringTeam first
    | (scoringTeam, first, second) <- setterPositions gameSetters servingOrder
    , setterInFront first
      && setterInBack second
      || setterInBack first
      && setterInFront second
    ]


  setterInBack :: Set Position -> Bool
  setterInBack s = any (\p -> row p == Back) s

  setterInFront :: Set Position -> Bool
  setterInFront s = not $ setterInBack s

  points :: Team -> Set Position -> (Int, Int)
  points team first = case (team, setterInFront first) of
    (First , True ) -> (1, 0)
    (First , False) -> (0, 1)
    (Second, True ) -> (0, 1)
    (Second, False) -> (1, 0)


-- | With a given boxscore url, return the tallies for all sets.
withBoxscore :: BoxscoreUrl -> IO [(Int, Int)]
withBoxscore boxscoreUrl = do
  sets        <- getPlays boxscoreUrl
  assistStats <- getAssists boxscoreUrl
  forM (zip [1..] sets) $ winningSetterRows (setters assistStats)
  

scheduleUrls :: [Url]
scheduleUrls =
  [ "https://clariongoldeneagles.com/sports/womens-volleyball/schedule/2021"
  , "https://wwuvikings.com/sports/womens-volleyball/schedule/2021"
  , "https://cspbears.com/sports/womens-volleyball/schedule/2021"
  , "https://wucardinals.com/sports/womens-volleyball/schedule/2021"
  , "https://gannonsports.com/sports/womens-volleyball/schedule/2021"
  , "https://csusbathletics.com/sports/womens-volleyball/schedule/2021"
  , "https://minesathletics.com/sports/womens-volleyball/schedule/2021"
  , "https://goargos.com/sports/womens-volleyball/schedule/2021"
  ]


main :: IO ()
main = do
  results <- forM scheduleUrls $ \scheduleUrl -> do
    boxscoreUrls <- getBoxscoreUrls scheduleUrl
    results'     <- forM boxscoreUrls withBoxscore
    pure $ concat results'

  let (f, b) = unzip $ concat results
  let fD     = fromInteger (toInteger $ sum f) :: Double
  let bD     = fromInteger (toInteger $ sum b) :: Double
  let total  = fD + bD
  let fP     = fD / total * 100
  let bP     = bD / total * 100
  putStrLn $ printf "total measured       :  %.0f" total
  putStrLn $ printf "total front row wins :  %.0f  %2.1f%%" fD fP
  putStrLn $ printf "total back row wins  :  %.0f  %2.1f%%" bD bP


