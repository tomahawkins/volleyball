module Main (main) where

import Data.List
import System.Environment
import Text.Printf

import Match
import Rankings

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["matches",        file] -> matches'  False file
    ["matches",  "-n", file] -> matches'  True  file
    ["rankings",       file] -> rankings' False file
    ["rankings", "-n", file] -> rankings' True  file
    _ -> return ()

-- Get the schedule urls from a file.
schedules :: FilePath -> IO [String]
schedules file = do
  f <- readFile file
  return [ head $ words l | l <- lines f, isPrefixOf "http" l ]

matches' :: Bool -> FilePath -> IO ()
matches' refetch file = do
  m <- schedules file >>= mapM (matches refetch)
  mapM_ print m

rankings' :: Bool -> FilePath -> IO ()
rankings' refetch file = do
  m <- schedules file >>= mapM (matches refetch)
  mapM_ (\ (t, r) -> printf "%-5s  %2.0f%%\n" t $ r * 100) $ rankings m
  


