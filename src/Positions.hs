module Positions
  ( positions
  , teamPlayersAll
  ) where

import Control.Monad
import Data.Maybe

import FDSolver
import Match
import Substitutions

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
  | Sub'    [a] [a] (Positions a)

instance Show a => Show (P a) where
  show a = case a of
    Sub' a b p -> "Sub: " ++ show a ++ " " ++ show b ++ "\n" ++ show p
    Volley' st sp wt v p -> "Volley: " ++ show st ++ " " ++ show sp ++ " " ++ show wt ++ " " ++ show v ++ "\n" ++ show p

instance Functor P where
  fmap f a = case a of
    Sub' a b p -> Sub' (fmap f a) (fmap f b) (fmap f p)
    Volley' a b c d e -> Volley' a b c d $ fmap f e

-- | Infers player positions of a team throughout a set.
positions :: Team -> Name -> [Name] -> Set -> IO [P (Var, [Name])]
positions team libero defense set = do
  --mapM_ print constraints
  --print $ convert libero
  print $ fmap convert fixed
  return $ map (fmap convert) p
  where
  ((fixed, p), convert, constraints) = solve' f
  f = do
    (fixed, p) <- initP team libero $ substitutions team libero set
    applyRotations team fixed p
    mapM_ (applyServer team libero) p
    applySubs team p
    --applyDefense defense p
    return (fixed, p)

p1 (Positions a) = a !! 0
p2 (Positions a) = a !! 1
p3 (Positions a) = a !! 2
p4 (Positions a) = a !! 3
p5 (Positions a) = a !! 4
p6 (Positions a) = a !! 5

ps = [p1, p2, p3, p4, p5, p6]

applyServer :: Team -> Name -> P Var -> FD Name ()
applyServer team libero a = case a of
  Volley' t (Just server) _ _ p
    | t == team && server /= libero -> do
        v <- newVar [server]
        always $ v :== p1 p
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
  Sub' _ _ p : b : rest -> do
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
  Sub' playersGoingIn playersGoingOut a : b' : rest -> do

    -- All subs are different players.
    allDifferent $ playersGoingIn ++ playersGoingOut

    -- Propagte non subing players.
    sequence_ [ always $ foldl1 (:&&) [ p b :/= s | s <- playersGoingIn  ] :-> p a :== p b | p <- ps ]
    sequence_ [ always $ foldl1 (:&&) [ p a :/= s | s <- playersGoingOut ] :-> p a :== p b | p <- ps ]

    -- Players going in (out) should not be in previous (next) rotation.
    sequence_ [ always $ s :/= p a | s <- playersGoingIn,  p <- ps ]
    sequence_ [ always $ s :/= p b | s <- playersGoingOut, p <- ps ]

    -- Players going in (out) should be in next (previous) rotation.
    sequence_ [ always $ foldl1 (:||) [ s :== p b | p <- ps ] | s <- playersGoingIn  ]
    sequence_ [ always $ foldl1 (:||) [ s :== p a | p <- ps ] | s <- playersGoingOut ]

    {-
    flip mapM_ playersGoingIn $ \ s -> do
      s <- newVar [s]
      always $ foldl1 (:||) [ s :== p b | p <- ps ]
      sequence_ [ always $ s :/= p a | p <- ps ]

    if not $ null playersGoingOut
      then do
        subsOut <- newVar playersGoingOut
        sequence_ [ always $ p a :/= subsOut :-> p a :== p b | p <- ps ]
        flip mapM_ playersGoingOut $ \ s -> do
          s <- newVar [s]
          always $ foldl1 (:||) [ s :== p a | p <- ps ]
          sequence_ [ always $ s :/= p b | p <- ps ]
      else do
        flip mapM_ playersGoingIn $ \ i -> do
          o <- newVar all
          always $ o :/= i
          sequence_ [ always $ o :/= p b | p <- ps ]  -- Out going player not in next rotation.
          always $ foldl1 (:||) [ o :== p a | p <- ps ]  -- Out going player somewhere in previous rotation.
          -- XXX Still need a way to propogate non subed players.
-}
    applySubs team $ b' : rest
    where
    b = positionsOf b'
  _ : rest -> applySubs team rest

applyDefense :: [Name] -> [P Var] -> FD Name ()
applyDefense defense p = do
  defense <- mapM (newVar . (:[])) defense
  flip mapM_ p $ \ p -> case p of
    Volley' _ _ _ _ a -> sequence_ [ always $ d :/= p a | d <- defense, p <- [p2, p3, p4] ]
    Sub' _ _ _ -> return ()

allDifferent :: [Var] -> FD Name ()
allDifferent a = sequence_ [ always $ m :/= n | m <- a, n <- a, m /= n ]

newPositions :: [Name] -> FD Name (Positions Var)
newPositions all = do
  p1 <- newVar all
  p2 <- newVar all
  p3 <- newVar all
  p4 <- newVar all
  p5 <- newVar all
  p6 <- newVar all
  allDifferent [p1, p2, p3, p4, p5]
  return $ Positions [p1, p2, p3, p4, p5, p6]

initP :: Team -> Name -> Set -> FD Name (Positions Var, [P Var])
initP team libero set@(Set events) = do
  fixed <- newPositions all
  p <- mapM f events >>= return . catMaybes
  return (fixed, p)
  where
  all  = filter (/= libero) $ teamPlayersAll team set
  f :: Event -> FD Name (Maybe (P Var))
  f a = case a of
    Timeout -> return Nothing
    Unknown _ -> return Nothing
    Sub t i o
      | t == team -> do
          p <- newPositions all
          i <- mapM (newVar . (:[])) i
          o <- if null o then replicateM (length i) (newVar all) else mapM (newVar . (:[])) o
          return $ Just $ Sub' i o p
      | otherwise -> return Nothing
    Volley a b c d -> do
      p <- newPositions all
      return $ Just $ Volley' a b c d p

