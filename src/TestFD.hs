module Main (main) where

import FDSolver

main :: IO ()
main = do
  --print (f a, f b, f c)
  print (f a, f b)

  where
  --((a, b, c), f) = solve test1
  ((a, b), f) = solve test2

test1 :: FD Int (Var, Var, Var)
test1 = do
  a <- newVar [1]
  b <- newVar [1, 2, 3]
  c <- newVar [1]
  assert $ c :== a :-> c :/= b
  return (a, b, c)

test2 :: FD Int (Var, Var)
test2 = do
  a <- newVar [1]
  b <- newVar [1, 2, 3]
  assert $ a :/= b
  return (a, b)

