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
  | Sub'    [a]
  deriving Eq

instance Show a => Show (P a) where
  show a = case a of
    Sub' a -> "Sub: " ++ show a ++ "\n"
    Volley' _ _ _ _ p -> show p

instance Functor P where
  fmap f a = case a of
    Sub' a -> Sub' $ map f a
    Volley' a b c d e -> Volley' a b c d $ fmap f e

-- | Infers player positions of a team throughout a set.
positions :: Team -> Name -> Set -> [P [Name]]
positions team libero set = map (fmap convert) p
  where
  (p, convert) = solve $ do
    p <- initP team libero set
    applyServers team libero p
    rotationsBetweenSubs team p
    rotationsAcrossSubs  team p
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

-- Propagate info between subs.
rotationsBetweenSubs :: Team -> [P Var] -> FD Name ()
rotationsBetweenSubs team a = case a of
  [] -> return ()
  Volley' st0 _ _ _ a : v@(Volley' st1 _ _ _ b) : rest -> do
    if st0 == st1 || st1 /= team
      then do
        assert $ p1 a :== p1 b 
        assert $ p2 a :== p2 b 
        assert $ p3 a :== p3 b 
        assert $ p4 a :== p4 b 
        assert $ p5 a :== p5 b 
        assert $ p6 a :== p6 b 
      else do
        assert $ p1 a :== p6 b 
        assert $ p2 a :== p1 b 
        assert $ p3 a :== p2 b 
        assert $ p4 a :== p3 b 
        assert $ p5 a :== p4 b 
        assert $ p6 a :== p5 b 
    rotationsBetweenSubs team $ v : rest
  _ : rest -> rotationsBetweenSubs team rest

-- Propagate info across subs.
rotationsAcrossSubs :: Team -> [P Var] -> FD Name ()
rotationsAcrossSubs team a = case a of
  [] -> return ()
  Volley' st0 _ _ _ a : Sub' subs : v@(Volley' st1 _ _ _ b) : rest -> do
    sequence_ [ assert $ sub :== f0 a :-> sub :/= f1 b | sub <- subs, f0 <- ps, f1 <- ps ]  -- XXX Does not work.
    sequence_ [ assert $ sub :== f0 b :-> sub :/= f1 a | sub <- subs, f0 <- ps, f1 <- ps ]
    rotationsBetweenSubs team $ v : rest
  _ : rest -> rotationsBetweenSubs team rest
  where
  ps = [p1, p2, p3, p4, p5, p6]

{-
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
        | t == team -> do
            a <- mapM (newVar . (:[])) a
            rest <- f rest
            return $ Sub' a : rest
        | otherwise -> f rest
      Volley a b c d -> do
        p1 <- newVar all
        p2 <- newVar all
        p3 <- newVar all
        p4 <- newVar all
        p5 <- newVar all
        p6 <- newVar all
        assert $ p1 :/= p2
        assert $ p1 :/= p3
        assert $ p1 :/= p4
        assert $ p1 :/= p5
        assert $ p1 :/= p6
        assert $ p2 :/= p3
        assert $ p2 :/= p4
        assert $ p2 :/= p5
        assert $ p2 :/= p6
        assert $ p3 :/= p4
        assert $ p3 :/= p5
        assert $ p3 :/= p6
        assert $ p4 :/= p5
        assert $ p4 :/= p6
        assert $ p5 :/= p6
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
       
