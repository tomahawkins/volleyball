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
    Volley _ _ PointAwarded -> []
    Volley _ t (KillBy _ a b c)
      | t == team -> a : maybeToList b
      | otherwise -> maybeToList c
    Volley _ t (AttackError _ a b)
      | t == team -> b
      | otherwise -> [a]
    Volley _ t (ServiceError a)
      | t == team -> []
      | otherwise -> [a]
    Volley _ t (ServiceAce a b)
      | t == team -> [a]
      | otherwise -> [b]
    Volley _ t (BallHandlingError _ a)
      | t == team -> []
      | otherwise -> [a]
    Volley _ t (BadSet _ a)
      | t == team -> []
      | otherwise -> [a]

