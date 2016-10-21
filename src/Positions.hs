module Positions
  ( positions
  , teamPlayersAll
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
  | Sub'    [Name] [a] (Positions a)
  deriving Eq

instance Show a => Show (P a) where
  show a = case a of
    Sub' a _ p -> "Sub: " ++ show a ++ "\n" ++ show p
    Volley' st sp wt v p -> "Volley: " ++ show st ++ " " ++ show sp ++ " " ++ show wt ++ " " ++ show v ++ "\n" ++ show p

instance Functor P where
  fmap f a = case a of
    Sub' a b p -> Sub' a (map f b) (fmap f p)
    Volley' a b c d e -> Volley' a b c d $ fmap f e

-- | Infers player positions of a team throughout a set.
positions :: Team -> Name -> Set -> IO [P (Var, [Name])]
positions team libero set = do
  mapM_ print constraints
  print $ fmap convert fixed
  return $ map (fmap convert) p
  where
  ((fixed, p), convert, constraints) = solve' f
  f = do
    (fixed, p) <- initP team libero set
    applyRotations team fixed p
    applyServers team libero p
    applySubs team [] p
    mapM_ (applyBlockers team) p
    return (fixed, p)

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

positionsOf :: P a -> Positions a
positionsOf a = case a of
  Volley' _ _ _ _ p -> p
  Sub' _ _ p -> p

applyRotations :: Team -> Positions Var -> [P Var] -> FD Name ()
applyRotations team fixed a = case a of
  [] -> return ()
  Volley' st _ wt _ a : b : rest -> do
    applyFixed a
    if st /= wt && wt == team
      then do
        rotate a $ positionsOf b
        applyRotations team (rotateP fixed) $ b : rest
      else do
        dontRotate a $ positionsOf b
        applyRotations team fixed $ b : rest
  Sub' _ _ p : rest -> do
    applyFixed p
    applyRotations team fixed rest
  Volley' _ _ _ _ p : rest -> do
    applyFixed p
    applyRotations team fixed rest
  where
  rotateP :: Positions Var -> Positions Var
  rotateP (Positions a) = Positions $ tail a ++ [head a]

  rotate :: Positions Var -> Positions Var -> FD Name ()
  rotate a b = do
    assert $ p1 a :== p6 b 
    assert $ p2 a :== p1 b 
    assert $ p3 a :== p2 b 
    assert $ p4 a :== p3 b 
    assert $ p5 a :== p4 b 
    assert $ p6 a :== p5 b 

  dontRotate :: Positions Var -> Positions Var -> FD Name ()
  dontRotate a b = do
    assert $ p1 a :== p1 b 
    assert $ p2 a :== p2 b 
    assert $ p3 a :== p3 b 
    assert $ p4 a :== p4 b 
    assert $ p5 a :== p5 b 
    assert $ p6 a :== p6 b 

  applyFixed :: Positions Var -> FD Name ()
  applyFixed p = do
    assert $ p1 p :/= p2 fixed
    assert $ p1 p :/= p3 fixed
    assert $ p1 p :/= p4 fixed
    assert $ p1 p :/= p5 fixed
    assert $ p1 p :/= p6 fixed

    assert $ p2 p :/= p1 fixed
    assert $ p2 p :/= p3 fixed
    assert $ p2 p :/= p4 fixed
    assert $ p2 p :/= p5 fixed
    assert $ p2 p :/= p6 fixed

    assert $ p3 p :/= p1 fixed
    assert $ p3 p :/= p2 fixed
    assert $ p3 p :/= p4 fixed
    assert $ p3 p :/= p5 fixed
    assert $ p3 p :/= p6 fixed

    assert $ p4 p :/= p1 fixed
    assert $ p4 p :/= p2 fixed
    assert $ p4 p :/= p3 fixed
    assert $ p4 p :/= p5 fixed
    assert $ p4 p :/= p6 fixed

    assert $ p5 p :/= p1 fixed
    assert $ p5 p :/= p2 fixed
    assert $ p5 p :/= p3 fixed
    assert $ p5 p :/= p4 fixed
    assert $ p5 p :/= p6 fixed

    assert $ p6 p :/= p1 fixed
    assert $ p6 p :/= p2 fixed
    assert $ p6 p :/= p3 fixed
    assert $ p6 p :/= p4 fixed
    assert $ p6 p :/= p5 fixed

applySubs :: Team -> [P Var] -> [P Var] -> FD Name ()
applySubs team before a = case a of
  [] -> return ()
  s@(Sub' subNames subs a) : b : rest -> do
    -- TODO: If a sub in referenced by volley info on one side, ensure the sub is not pressent on the other.
    playersGoingIn  <- mapM (newVar . (:[])) $ subNames `intersect` teamPlayersUntilNextSub team rest
    playersGoingOut <- mapM (newVar . (:[])) $ subNames `intersect` teamPlayersUntilNextSub team before
    sequence_ [ assert $ sub :/= p a  | sub <- playersGoingIn,  p <- ps ]
    sequence_ [ assert $ sub :/= p b' | sub <- playersGoingOut, p <- ps ]

    -- If a sub is seen on one side, make sure it does not appear on the other.
    sequence_ [ assert $ sub :== f0 a  :-> sub :/= f1 b' | sub <- subs, f0 <- ps, f1 <- ps ]
    sequence_ [ assert $ sub :== f0 b' :-> sub :/= f1 a  | sub <- subs, f0 <- ps, f1 <- ps ]

    -- If a position is none of the subs on both sides, bind the two sides together for that position.
    flip mapM_ ps $ \ p -> assert $ foldl1 (:&&) [ s :/= p a :&& s :/= p b' | s <- subs ] :-> p a :== p b'

    applySubs team (s : before) (b : rest)
    where
    b' = positionsOf b

  a : rest -> applySubs team (a : before) rest
  where
  ps = [p1, p2, p3, p4, p5, p6]

applyBlockers :: Team -> P Var -> FD Name ()
applyBlockers team a = case a of
  Volley' _ _ t (KillBy _ _ (Just blocker)) p
    | t /= team -> applyBlocker blocker p
  Volley' _ _ t (AttackError _ blockers) p
    | t == team -> mapM_ (flip applyBlocker p) blockers
  _ -> return ()

applyBlocker :: Name -> Positions Var -> FD Name ()
applyBlocker blocker p = do
  blocker <- newVar [blocker]
  assert $ blocker :== p2 p :|| blocker :== p3 p :|| blocker :== p4 p
  assert $ blocker :/= p5 p
  assert $ blocker :/= p6 p
  assert $ blocker :/= p1 p

newPositions :: [Name] -> FD Name (Positions Var)
newPositions all = do
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
  return $ Positions [p1, p2, p3, p4, p5, p6]

initP :: Team -> Name -> Set -> FD Name (Positions Var, [P Var])
initP team libero set@(Set events) = do
  a <- newPositions all
  p <- f events
  return (a, p)
  where
  all = filter (/= libero) $ teamPlayersAll team set
  f :: [Event] -> FD Name [P Var]
  f a = case a of
    [] -> return []
    a : rest -> case a of
      Timeout -> f rest
      Unknown _ -> f rest
      Sub t a
        | t == team -> do
            b <- mapM (newVar . (:[])) a
            p <- newPositions all
            rest <- f rest
            return $ Sub' a b p : rest
        | otherwise -> f rest
      Volley a b c d -> do
        p <- newPositions all
        rest <- f rest
        return $ Volley' a b c d p : rest
      


teamPlayersAll :: Team -> Set -> [Name]
teamPlayersAll team (Set events) = nub $ concatMap f events
  where
  f a = case a of
    Timeout -> []
    Unknown _ -> []
    Sub t a
      | t == team -> a
      | otherwise -> []
    Volley st sp wt v -> teamPlayersVolley team st sp wt v

teamPlayersUntilNextSub :: Team -> [P Var] -> [Name]
teamPlayersUntilNextSub team = nub . f
  where
  f a = case a of
    [] -> []
    Volley' st sp wt v _ : rest -> teamPlayersVolley team st sp wt v ++ f rest
    Sub' _ _ _ : _ -> []

teamPlayersVolley :: Team -> Team -> Maybe Name -> Team -> Volley -> [Name]  -- Team of interest, serving team, serving player, winning team, volley.
teamPlayersVolley team st sp wt a = (if st == team then maybeToList sp else []) ++ case a of
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


