{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import           Data.Text                      ( Text )
--import Text.HTML.TagSoup ()


main :: IO ()
main = do
  print $ winningSetterRows exampleSet
  print $ winningSetterRows example5thSet


winningSetterRows :: [Text] -> (Int, Int)
winningSetterRows names = (frontWins, backWins)
 where
  rows = winningSetterRow
    <$> settersInDifferentRows (servingScoringTeamsSetterPositions names)
  frontWins = length [ () | r <- rows, r == Front ]
  backWins  = length rows - frontWins


settersInDifferentRows
  :: [(Team, Team, (Position, Position))] -> [(Team, Team, (Row, Row))]
settersInDifferentRows a =
  [ (serve, point, (row p1, row p2))
  | (serve, point, (p1, p2)) <- a
  , row p1 /= row p2
  ]


winningSetterRow :: (Team, Team, (Row, Row)) -> Row
winningSetterRow (_, pointTeam, (firstRow, secondRow)) = case pointTeam of
  First  -> firstRow
  Second -> secondRow


servingScoringTeamsSetterPositions
  :: [Text] -> [(Team, Team, (Position, Position))]
servingScoringTeamsSetterPositions names =
  setterPositions $ servingScoringTeams names


servingScoringTeams :: [Text] -> [(Team, Team)]
servingScoringTeams names =
  zip (servingTeams names) (scoringTeams $ servingTeams names)


-- | Setter positions of first and second teams.
setterPositions :: [(Team, Team)] -> [(Team, Team, (Position, Position))]
setterPositions servingScoring = setterPositions' (P1, P2) servingScoring


setterPositions'
  :: (Position, Position)
  -> [(Team, Team)]
  -> [(Team, Team, (Position, Position))]
setterPositions' p@(firstP, secondP) servingScoring = case servingScoring of
  []                       -> []

  [(serveTeam, pointTeam)] -> [(serveTeam, pointTeam, p)]

  (serveTeam0, pointTeam0) : rest@((serveTeam1, _pointTeam1) : _) ->
    (serveTeam0, pointTeam0, p) : setterPositions' p' rest

   where

    p'
      | serveTeam0 == serveTeam1 = p
      | otherwise = case serveTeam1 of
        First  -> (rotatePosition firstP, secondP)
        Second -> (firstP, rotatePosition secondP)


-- | Floor position.  Server at P1.
data Position = P1 | P2 | P3 | P4 | P5 | P6 deriving Show


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


-- | Rotate a position.
rotatePosition :: Position -> Position
rotatePosition = \case
  P1 -> P6
  P6 -> P5
  P5 -> P4
  P4 -> P3
  P3 -> P2
  P2 -> P1


-- | Which team serves first and second.
data Team = First | Second deriving (Show, Eq)


-- | Serving teams (First/Second) from team names serving sequence.
servingTeams :: [Text] -> [Team]
servingTeams = \case
  []       -> []
  a : rest -> First : (whichTeam <$> rest)

   where

    whichTeam t | t == a    = First
                | otherwise = Second


-- | Scoring teams from serving teams.
scoringTeams :: [Team] -> [Team]
scoringTeams serving = scoringTeams' (0, 0) serving


scoringTeams' :: (Int, Int) -> [Team] -> [Team]
scoringTeams' (firstPoints, secondPoints) teams = case teams of
  [] -> []

  _ : rest@(First : _) ->
    First : scoringTeams' (firstPoints + 1, secondPoints) rest

  _ : rest@(Second : _) ->
    Second : scoringTeams' (firstPoints, secondPoints + 1) rest

  [_] | firstPoints > secondPoints -> [First]
      | otherwise                  -> [Second]


example5thSet :: [Text]
example5thSet =
  [ "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "IUP"
  , "IUP"
  , "IUP"
  , "Clar"
  , "IUP"
  , "Clar"
  , "IUP"
  , "Clar"
  , "Clar"
  , "Clar"
  , "Clar"
  , "IUP"
  , "IUP"
  , "IUP"
  , "Clar"
  , "IUP"
  , "IUP"
  , "Clar"
  , "IUP"
  ]


exampleSet :: [Text]
exampleSet =
  [ "IUP"
  , "IUP"
  , "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "Clar"
  , "Clar"
  , "Clar"
  , "Clar"
  , "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "IUP"
  , "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "Clar"
  , "IUP"
  , "Clar"
  , "Clar"
  , "Clar"
  , "IUP"
  , "Clar"
  , "IUP"
  , "Clar"
  , "IUP"
  , "Clar"
  , "IUP"
  , "Clar"
  , "Clar"
  , "IUP"
  , "IUP"
  , "Clar"
  , "IUP"
  , "IUP"
  , "IUP"
  , "Clar"
  , "IUP"
  , "IUP"
  , "IUP"
  , "IUP"
  , "Clar"
  , "IUP"
  ]


