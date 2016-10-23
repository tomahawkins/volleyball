module Positions
  ( positions
  , teamPlayersAll
  ) where

import Data.List
import Data.Maybe
import Debug.Trace

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
  | Sub'    [Name] (Positions a)
  deriving Eq

instance Show a => Show (P a) where
  show a = case a of
    Sub' a p -> "Sub: " ++ show a ++ "\n" ++ show p
    Volley' st sp wt v p -> "Volley: " ++ show st ++ " " ++ show sp ++ " " ++ show wt ++ " " ++ show v ++ "\n" ++ show p

instance Functor P where
  fmap f a = case a of
    Sub' a p -> Sub' a (fmap f p)
    Volley' a b c d e -> Volley' a b c d $ fmap f e

-- | Infers player positions of a team throughout a set.
positions :: Team -> Name -> Set -> IO [P (Var, [Name])]
positions team libero set = do
  mapM_ print constraints
  --print $ convert libero
  print $ fmap convert fixed
  return $ map (fmap convert) p
  where
  ((fixed, p), convert, constraints) = solve' f
  f = do
    (fixed, p) <- initP team libero set
    applyRotations team fixed p
    mapM_ (applyKnownPlayers team libero) p
    applySubs team p
    return (fixed, p)

p1 (Positions a) = a !! 0
p2 (Positions a) = a !! 1
p3 (Positions a) = a !! 2
p4 (Positions a) = a !! 3
p5 (Positions a) = a !! 4
p6 (Positions a) = a !! 5

applyKnownPlayers :: Team -> Name -> P Var -> FD Name ()
applyKnownPlayers team libero a = do
  applyServer
  --applyBlockers
  case a of
    Sub' _ _ -> return ()
    Volley' st sp wt v p -> mapM_ (flip applyPlayer p) $ filter (/= libero) $ teamPlayersVolley team st sp wt v
  where
  applyPlayer :: Name -> Positions Var -> FD Name ()
  applyPlayer player p = do
    player <- newVar [player]
    always $ player :== p1 p
         :|| player :== p2 p
         :|| player :== p3 p
         :|| player :== p4 p
         :|| player :== p5 p
         :|| player :== p6 p

  applyServer :: FD Name ()
  applyServer = case a of
    Volley' t (Just server) _ _ p
      | t == team && server /= libero -> do
          v <- newVar [server]
          always $ v :== p1 p
    _ -> return ()
  
  applyBlockers :: FD Name ()
  applyBlockers = case a of
    Volley' _ _ t (KillBy _ _ (Just blocker)) p
      | t /= team -> applyBlocker blocker p
    Volley' _ _ t (AttackError _ blockers) p
      | t == team -> mapM_ (flip applyBlocker p) blockers
    _ -> return ()
  
  applyBlocker :: Name -> Positions Var -> FD Name ()
  applyBlocker blocker p = do
    blocker <- newVar [blocker]
    always $ blocker :== p2 p :|| blocker :== p3 p :|| blocker :== p4 p
    always $ blocker :/= p5 p
    always $ blocker :/= p6 p
    always $ blocker :/= p1 p

positionsOf :: P a -> Positions a
positionsOf a = case a of
  Volley' _ _ _ _ p -> p
  Sub' _ p -> p

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
  Sub' _ p : b : rest -> do
    applyFixed p
    applyRotations team fixed $ b : rest
  [a] -> applyFixed (positionsOf a)
  where
  rotateP :: Positions Var -> Positions Var
  rotateP (Positions a) = Positions $ tail a ++ [head a]

  rotate :: Positions Var -> Positions Var -> FD Name ()
  rotate a b = do
    always $ p1 a :== p6 b 
    always $ p2 a :== p1 b
    always $ p3 a :== p2 b 
    always $ p4 a :== p3 b 
    always $ p5 a :== p4 b 
    always $ p6 a :== p5 b 

  dontRotate :: Positions Var -> Positions Var -> FD Name ()
  dontRotate a b = do
    usually $ p1 a :== p1 b 
    usually $ p2 a :== p2 b 
    usually $ p3 a :== p3 b 
    usually $ p4 a :== p4 b 
    usually $ p5 a :== p5 b 
    usually $ p6 a :== p6 b 

  applyFixed :: Positions Var -> FD Name ()
  applyFixed p = do
    always $ p1 p :/= p2 fixed
    always $ p1 p :/= p3 fixed
    always $ p1 p :/= p4 fixed
    always $ p1 p :/= p5 fixed
    always $ p1 p :/= p6 fixed
    always $ p2 p :/= p1 fixed
    always $ p2 p :/= p3 fixed
    always $ p2 p :/= p4 fixed
    always $ p2 p :/= p5 fixed
    always $ p2 p :/= p6 fixed
    always $ p3 p :/= p1 fixed
    always $ p3 p :/= p2 fixed
    always $ p3 p :/= p4 fixed
    always $ p3 p :/= p5 fixed
    always $ p3 p :/= p6 fixed
    always $ p4 p :/= p1 fixed
    always $ p4 p :/= p2 fixed
    always $ p4 p :/= p3 fixed
    always $ p4 p :/= p5 fixed
    always $ p4 p :/= p6 fixed
    always $ p5 p :/= p1 fixed
    always $ p5 p :/= p2 fixed
    always $ p5 p :/= p3 fixed
    always $ p5 p :/= p4 fixed
    always $ p5 p :/= p6 fixed
    always $ p6 p :/= p1 fixed
    always $ p6 p :/= p2 fixed
    always $ p6 p :/= p3 fixed
    always $ p6 p :/= p4 fixed
    always $ p6 p :/= p5 fixed

applySubs :: Team -> [P Var] -> FD Name ()
applySubs team a = case a of
  [] -> return ()
  Sub' subs a : b : rest -> do
    subsOut <- newVar playersGoingOut
    always $ p1 a :/= subsOut :-> p1 a :== p1 b'
    always $ p2 a :/= subsOut :-> p2 a :== p2 b'
    always $ p3 a :/= subsOut :-> p3 a :== p3 b'
    always $ p4 a :/= subsOut :-> p4 a :== p4 b'
    always $ p5 a :/= subsOut :-> p5 a :== p5 b'
    always $ p6 a :/= subsOut :-> p6 a :== p6 b'

    subsIn <- newVar playersGoingIn
    always $ p1 b' :/= subsIn :-> p1 a :== p1 b'
    always $ p2 b' :/= subsIn :-> p2 a :== p2 b'
    always $ p3 b' :/= subsIn :-> p3 a :== p3 b'
    always $ p4 b' :/= subsIn :-> p4 a :== p4 b'
    always $ p5 b' :/= subsIn :-> p5 a :== p5 b'
    always $ p6 b' :/= subsIn :-> p6 a :== p6 b'

    flip mapM_ playersGoingIn $ \ p -> do
      p <- newVar [p]
      always $ p :== p1 b'
           :|| p :== p2 b'
           :|| p :== p3 b'
           :|| p :== p4 b'
           :|| p :== p5 b'
           :|| p :== p6 b'

      always $ p :/= p1 a
      always $ p :/= p2 a
      always $ p :/= p3 a
      always $ p :/= p4 a
      always $ p :/= p5 a
      always $ p :/= p6 a

    flip mapM_ playersGoingOut $ \ p -> do
      p <- newVar [p]
      always $ p :== p1 a
           :|| p :== p2 a
           :|| p :== p3 a
           :|| p :== p4 a
           :|| p :== p5 a
           :|| p :== p6 a

      always $ p :/= p1 b'
      always $ p :/= p2 b'
      always $ p :/= p3 b'
      always $ p :/= p4 b'
      always $ p :/= p5 b'
      always $ p :/= p6 b'

    applySubs team $ b : rest

    --XXX
    {-
    -- If a sub in referenced by volley info on one side, ensure the sub is not pressent on the other.
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
    -}
    where
    b' = positionsOf b
    playersGoingIn :: [Name]
    playersGoingIn  = everyOther subs
    playersGoingOut = trace ("subs: " ++ show subs ++ show (everyOther subs) ++ show (everyOther $ tail subs)) everyOther $ tail subs
    everyOther :: [a] -> [a]
    everyOther a = case a of
      [] -> []
      a : _ : rest -> a : everyOther rest
      [a] -> [a]

  _ : rest -> applySubs team rest

newPositions :: [Name] -> FD Name (Positions Var)
newPositions all = do
  p1 <- newVar all
  p2 <- newVar all
  p3 <- newVar all
  p4 <- newVar all
  p5 <- newVar all
  p6 <- newVar all
  always $ p1 :/= p2
  always $ p1 :/= p3
  always $ p1 :/= p4
  always $ p1 :/= p5
  always $ p1 :/= p6
  always $ p2 :/= p3
  always $ p2 :/= p4
  always $ p2 :/= p5
  always $ p2 :/= p6
  always $ p3 :/= p4
  always $ p3 :/= p5
  always $ p3 :/= p6
  always $ p4 :/= p5
  always $ p4 :/= p6
  always $ p5 :/= p6
  return $ Positions [p1, p2, p3, p4, p5, p6]

initP :: Team -> Name -> Set -> FD Name (Positions Var, [P Var])
initP team libero set@(Set events) = do
  fixed <- newPositions all

  p <- mapM f events >>= return . catMaybes
  return (fixed, p)
  where
  all  = filter (/= libero) $ teamPlayersAll team set
  --subs = teamPlayersSubs team set
  f :: Event -> FD Name (Maybe (P Var))
  f a = case a of
    Timeout -> return Nothing
    Unknown _ -> return Nothing
    Sub t a
      | t == team -> do
          p <- newPositions all
          return $ Just $ Sub' a p
      | otherwise -> return Nothing
    Volley a b c d -> do
      p <- newPositions all
      return $ Just $ Volley' a b c d p

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

teamPlayersSubs :: Team -> Set -> [Name]
teamPlayersSubs team (Set events) = nub $ concat [ a | Sub t a <- events, t == team ]
      
{-
teamPlayersUntilNextSub :: Team -> [P Var] -> [Name]
teamPlayersUntilNextSub team = nub . f
  where
  f a = case a of
    [] -> []
    Volley' st sp wt v _ : rest -> teamPlayersVolley team st sp wt v ++ f rest
    --Sub' _ _ _ : _ -> []
-}

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


