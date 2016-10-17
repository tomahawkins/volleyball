module Main (main) where

import Data.List
import System.Environment
import System.IO
import Text.Printf

import Match
import Rankings

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["season",         file] -> season'   False file
    ["season",   "-n", file] -> season'   True  file

    ["rankings", file] -> do
      f <- readFile file
      mapM_ (\ (t, r) -> printf "%-5s  %2.2f%%\n" t $ r * 100) $ rankings $ parseSeasons f

    ["load", file] -> do
      f <- readFile file
      mapM_ (putStr . show) $ parseSeasons f

    _ -> return ()

-- Get the schedule urls from a file.
schedules :: FilePath -> IO [String]
schedules file = do
  f <- readFile file
  return [ head $ words l | l <- lines f, isPrefixOf "http" l ]

season' :: Bool -> FilePath -> IO ()
season' refetch file = schedules file >>= mapM_ (\ f -> season refetch f >>= putStr . show >> hFlush stdout)

