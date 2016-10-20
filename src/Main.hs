module Main (main) where

import Data.List
import System.Environment
import System.IO
import Text.Printf

import Boxscores
import Match
import Positions
import Rankings

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["create-db",       file] -> schedules file >>= mapM_ (\ f -> boxscores False f >>= putStr . show >> hFlush stdout)
    ["create-db", "-f", file] -> schedules file >>= mapM_ (\ f -> boxscores True  f >>= putStr . show >> hFlush stdout)

    ["print-db", file] -> readFile file >>= mapM_ (putStr . show) . parseSeasons

    ["rankings", file] -> do
      f <- readFile file
      mapM_ (\ (t, r) -> printf "%-5s  %2.2f%%\n" t $ r * 100) $ rankings $ parseSeasons f

    ["test", file] -> readFile file >>= test . parseSeasons

    _ -> return ()

-- Get the schedule urls from a file.
schedules :: FilePath -> IO [String]
schedules file = do
  f <- readFile file
  return [ head $ words l | l <- lines f, isPrefixOf "http" l ]

test :: [Season] -> IO ()
test seasons = do
  --mapM_ print volleys
  --mapM_ print $ nub $ concatMap (teamPlayers "CLAR") sets
  mapM_ print $ positions "CLAR" "Catherine Ferragonio" set1
  where
  --sets = concat [ concat [ sets | Match _ sets <- m ] | Season "CLAR" m <- seasons ]
  Match _ sets = last $ head [ m | Season "CLAR" m <- seasons ]
  set1 : _ = sets

