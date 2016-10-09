module Main (main) where

import Data.List
import System.Environment

import Match

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["matches", file] -> do
      f <- readFile file
      m <- mapM matches [ head $ words l | l <- lines f, isPrefixOf "http" l ]
      mapM_ printMatch $ concat m
    _ -> return ()

printMatch :: Match -> IO ()
printMatch m = flip mapM_ (zip [1 ..] m) $ \ (n, s) -> do
  putStrLn $ "Set " ++ show n
  mapM_ print s
  putStrLn ""

