module Positions
  ( positions
  , teamPlayers
  ) where

import Data.List
import Data.Maybe

import Match

data Position a = Position { p1, p2, p3, p4, p5, p6 :: a }

-- | Infers player positions of a team throughout set.
positions :: Team -> Set -> [(Team, Position [Name])]
positions team (Set volleys) = undefined

teamPlayers :: Team -> Set -> [Name]
teamPlayers team (Set events) = nub $ concatMap f events
  where
  f a = case a of
    Timeout -> []
    Unknown _ -> []
    Sub t a
      | t == team -> a
      | otherwise -> []
    Volley st sp wt v -> (if st == team then maybeToList sp else []) ++ case v of
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
       
