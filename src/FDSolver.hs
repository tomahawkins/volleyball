{-# LANGUAGE GADTs #-}

-- | A finite domain constraint solver.
module FDSolver
  ( FD
  , Var
  , E (..)
  , solve
  , newVar
  , assert
  ) where

import Data.List
import MonadLib

-- | The FD monad.
type FD a = StateT (FDDB a) Id

data FDDB a = FDDB
  { nextId      :: Int
  , variables   :: [[a]]  -- Vars index into list.
  , constraints :: [E Bool]
  }

-- | Variable.
data Var = Var Int

-- | Constraint expressions.
data E a where
  (:&&)  :: E Bool -> E Bool -> E Bool
  (:||)  :: E Bool -> E Bool -> E Bool
  (:->)  :: E Bool -> E Bool -> E Bool
  (:==)  :: Var -> Var -> E Bool
  (:/=)  :: Var -> Var -> E Bool

infix  4 :==, :/=
infixl 3 :&&
infixl 2 :||
infixr 1 :->

-- | Solve a set of constraints and update a structure with solved domain values.
solve :: Eq a => FD a b -> (b, Var -> [a])
solve fd =  (b, \ (Var i) -> solvedVars !! i)
  where
  (b, db) = runId $ runStateT (FDDB 0 [] []) fd
  solvedVars = f0 (constraints db) $ variables db

f0 :: Eq a => [E Bool] -> [[a]] -> [[a]]
f0 constraints vars
  | vars == vars' = vars
  | otherwise     = f0 constraints vars'
  where
  vars' = foldr f1 vars constraints

f1 :: Eq a => E Bool -> [[a]] -> [[a]]
f1 a vars = case a of
  Var a :== Var b -> replace n a $ replace n b vars where n = (vars !! a) `intersect` (vars !! b)
  Var a :/= Var b -> replace n a $ replace n b vars where n = ((vars !! a) `union` (vars !! b)) \\ ((vars !! a) `intersect` (vars !! b))
  _ -> vars
  where
  replace a i l = take i l ++ [a] ++ drop (i + 1) l

-- | Create a new variables with an initial domain.
newVar :: [a] -> FD a Var
newVar domain = do
  if null domain then error "Variables can't have empty domain." else return ()
  db <- get
  set db { nextId = nextId db + 1, variables = variables db ++ [domain] }
  return $ Var $ nextId db

-- | Add a constraint.
assert :: E Bool -> FD a ()
assert a = do
  db <- get
  set db { constraints = constraints db ++ [a] }


