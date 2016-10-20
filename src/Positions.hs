module Positions
  ( positions
  , teamPlayers
  ) where

import Data.List
import Data.Maybe

import FDSolver
import Match

data Positions a = Positions [a] deriving Eq

instance Show a => Show (Positions a) where
  show (Positions p) = unlines
    [ "p1: " ++ show (p !! 0)
    , "p2: " ++ show (p !! 1)
    , "p3: " ++ show (p !! 2)
    , "p4: " ++ show (p !! 3)
    , "p5: " ++ show (p !! 4)
    , "p6: " ++ show (p !! 5)
    ]

instance Functor Positions where
  fmap f (Positions a) = Positions $ map f a


data P a
  = Volley' Team (Maybe Name) Team Volley (Positions a)
  | Sub'    [Name]
  deriving Eq

instance Show a => Show (P a) where
  show a = case a of
    Sub' a -> "Sub: " ++ show a ++ "\n"
    Volley' _ _ _ _ p -> show p

instance Functor P where
  fmap f a = case a of
    Sub' a -> Sub' a
    Volley' a b c d e -> Volley' a b c d $ fmap f e

-- | Infers player positions of a team throughout a set.
positions :: Team -> Name -> Set -> [P [Name]]
positions team libero set = map (fmap convert) p
  where
  (p, convert) = solve $ do
    p <- initP team libero set
    applyServers team libero p
    return p

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

p1 (Positions a) = a !! 0
p2 (Positions a) = a !! 1
p3 (Positions a) = a !! 2
p4 (Positions a) = a !! 3
p5 (Positions a) = a !! 4
p6 (Positions a) = a !! 5

applyServers :: Team -> Name -> [P Var] -> FD Name ()
applyServers team libero = mapM_ $ \ a -> case a of
  Volley' t (Just server) _ _ p
    | t == team && server /= libero -> do
        v <- newVar [server]
        assert $ v :== p1 p
    -- | t == team && server /= libero && elem server (p1 p) -> Volley' t (Just server) b c p { p1 = [server] }
    -- | t == team && server /= libero -> error $ "applyServers: " ++ t ++ " " ++ server ++ " " ++ show (p1 p)
  _ -> return ()

{-
rotateF :: Positions a -> Positions a
rotateF (Positions p) = Positions $ tail p ++ [head p]

rotateB :: Positions a -> Positions a
rotateB (Positions p1 p2 p3 p4 p5 p6) = Position p6 p1 p2 p3 p4 p5
-}

{-
unify :: Position [Name] -> Position [Name] -> Position [Name]
unify (Position a1 a2 a3 a4 a5 a6) (Position b1 b2 b3 b4 b5 b6) = Position (i a1 b1) (i a2 b2) (i a3 b3) (i a4 b4) (i a5 b5) (i a6 b6)
  where
  i = intersect

propagate :: Team -> [P] -> [P]
propagate team a
  | a == propagateSubs (propagateRotations team a) = a
  | otherwise = propagate team $ propagateSubs $ propagateRotations team a

-- Propagate info across subs.
propagateSubs :: [P] -> [P]
propagateSubs a = case a of
  [] -> []
  Volley' a1 a2 a3 a4 a5 : Sub' subs : Volley' b1 b2 b3 b4 b5 : rest -> Volley' a1 a2 a3 a4 a5' : Sub' subs : propagateSubs (Volley' b1 b2 b3 b4 b5' : rest)
    where
    a5' = filterKnowns (subs `intersect` knowns b5) a5
    b5' = filterKnowns (subs `intersect` knowns a5) b5
  a : b -> a : propagateSubs b

-- Known players in a fixed position.
knowns :: Position [Name] -> [Name]
knowns (Position p1 p2 p3 p4 p5 p6) = flip concatMap [p1, p2, p3, p4, p5, p6] $ \ a -> if length a == 1 then a else []

filterKnowns :: [Name] -> Position [Name] -> Position [Name]
filterKnowns knowns (Position p1 p2 p3 p4 p5 p6) = Position (f p1) (f p2) (f p3) (f p4) (f p5) (f p6)
  where
  f a
    | length a == 1 = a
    | otherwise     = filter (flip notElem knowns) a

filterKnowns' :: Position [Name] -> Position [Name]
filterKnowns' a = filterKnowns (knowns a) a

-- Propagate info between subs.
propagateRotations :: Team -> [P] -> [P]
propagateRotations team = f
  where
  f :: [P] -> [P]
  f a = case a of
    [] -> []
    Volley' st0 s0 wt0 v0 p0 : Volley' st1 s1 wt1 v1 p1 : rest
      | st0 == st1 || st1 /= team -> Volley' st0 s0 wt0 v0 p   : f (Volley' st1 s1 wt1 v1 p   : rest)
      | otherwise                 -> Volley' st0 s0 wt0 v0 p0' : f (Volley' st1 s1 wt1 v1 p1' : rest)
      where
      p   = filterKnowns' $ unify p0 p1
      p0' = filterKnowns' $ unify p0 (rotateB p1)
      p1' = filterKnowns' $ unify (rotateF p0) p1
    a : b -> a : f b

extractPositions :: [P] -> [Position [Name]]
extractPositions = mapMaybe $ \ a -> case a of
  Sub' _ -> Nothing
  Volley' _ _ _ _ a -> Just a
-}

initP :: Team -> Name -> Set -> FD Name [P Var]
initP team libero set@(Set events) = f events
  where
  all = filter (/= libero) $ teamPlayers team set
  f :: [Event] -> FD Name [P Var]
  f a = case a of
    [] -> return []
    a : rest -> case a of
      Timeout -> f rest
      Unknown _ -> f rest
      Sub t a
        | t == team -> f rest >>= return . (Sub' a :)
        | otherwise -> f rest
      Volley a b c d -> do
        p1 <- newVar all
        p2 <- newVar all
        p3 <- newVar all
        p4 <- newVar all
        p5 <- newVar all
        p6 <- newVar all
        rest <- f rest
        return $ Volley' a b c d (Positions [p1, p2, p3, p4, p5, p6]) : rest
      


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
       
