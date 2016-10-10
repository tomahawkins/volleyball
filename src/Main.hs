module Main (main) where

import Data.List
import System.Environment

import Match

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["matches", file] -> matches' False file
    ["matches", "-n", file] -> matches' True file
    _ -> return ()

matches' :: Bool -> FilePath -> IO ()
matches' refetch schedulesFile = do
  f <- readFile schedulesFile
  m <- mapM (matches refetch) [ head $ words l | l <- lines f, isPrefixOf "http" l ]
  mapM_ (putStr . show) $ concat m

