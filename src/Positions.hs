module Positions
  ( positions
  , teamPlayers
  ) where

import Data.List
import Data.Maybe

import Match

data Position a = Position { p1, p2, p3, p4, p5, p6 :: a } deriving Eq

instance Show a => Show (Position a) where
  show (Position p1 p2 p3 p4 p5 p6) = unlines
    [ "p1: " ++ show p1
    , "p2: " ++ show p2
    , "p3: " ++ show p3
    , "p4: " ++ show p4
    , "p5: " ++ show p5
    , "p6: " ++ show p6
    ]

-- | Infers player positions of a team throughout set.
positions :: Team -> Set -> [Position [Name]]
positions team set = extractPositions . propagate team . applyServers team $ initP team set

data P
  = Volley' Team (Maybe Name) Team Volley (Position [Name])
  | Sub'    [(Name, Name)]
  deriving Eq

{-
pruneSubs :: [P] -> [P]
pruneSubs = reverse . f [] . reverse
  where
  f :: [Name] -> [P] -> [P]
  f subs a = case a of
    [] -> []
    Sub' a : rest -> f a rest
    Volley' a b c d (Position p1 p2 p3 p4 p5 p6) : rest -> Volley' a b c d (Position (pr p1) (pr p2) (pr p3) (pr p4) (pr p5) (pr p6)) : f subs rest
    where
    pr = filter $ flip notElem subs
    -}

applyServers :: Team -> [P] -> [P]
applyServers team = map $ \ a -> case a of
  Volley' t (Just server) b c p
    | t == team && elem server (p1 p) -> Volley' t (Just server) b c p { p1 = [server] }
    | t == team -> error $ "applyServers: " ++ t ++ " " ++ server ++ " " ++ show (p1 p)
  a -> a

rotateF :: Position a -> Position a
rotateF (Position p1 p2 p3 p4 p5 p6) = Position p2 p3 p4 p5 p6 p1

rotateB :: Position a -> Position a
rotateB (Position p1 p2 p3 p4 p5 p6) = Position p6 p1 p2 p3 p4 p5

unify :: Position [Name] -> Position [Name] -> Position [Name]
unify (Position a1 a2 a3 a4 a5 a6) (Position b1 b2 b3 b4 b5 b6) = Position (i a1 b1) (i a2 b2) (i a3 b3) (i a4 b4) (i a5 b5) (i a6 b6)
  where
  i = intersect

propagate :: Team -> [P] -> [P]
propagate team a
  | a == f a  = a
  | otherwise = propagate team $ f a
  where
  f :: [P] -> [P]
  f a = case a of
    [] -> []
    Volley' st0 s0 wt0 v0 p0 : Volley' st1 s1 wt1 v1 p1 : rest
      | st0 == st1 || st1 /= team -> Volley' st0 s0 wt0 v0 p   : f (Volley' st1 s1 wt1 v1 p   : rest)
      | otherwise                 -> Volley' st0 s0 wt0 v0 p0' : f (Volley' st1 s1 wt1 v1 p1' : rest)
      where
      p   = unify p0 p1
      p0' = unify p0 (rotateB p1)
      p1' = unify (rotateF p0) p1
    a : b -> a : f b

extractPositions :: [P] -> [Position [Name]]
extractPositions = mapMaybe $ \ a -> case a of
  Sub' _ -> Nothing
  Volley' _ _ _ _ a -> Just a

initP :: Team -> Set -> [P]
initP team set@(Set events) = f events
  where
  all = teamPlayers team set
  pos = Position { p1 = all, p2 = all, p3 = all, p4 = all, p5 = all, p6 = all }

  f :: [Event] -> [P]
  f a = case a of
    [] -> []
    a : rest -> case a of
      Timeout -> f rest
      Unknown _ -> f rest
      Sub t a
        | t == team -> Sub' a : f rest
        | otherwise -> f rest
      Volley a b c d -> Volley' a b c d pos : f rest
      




teamPlayers :: Team -> Set -> [Name]
teamPlayers team (Set events) = nub $ concatMap f events
  where
  f a = case a of
    Timeout -> []
    Unknown _ -> []
    Sub t a
      | t == team -> c ++ d
      | otherwise -> []
      where (c, d) = unzip a
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
       
