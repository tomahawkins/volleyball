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
      mapM_ printMatch [ head $ words l | l <- lines f, isPrefixOf "http" l ]
    _ -> return ()

printMatch :: String -> IO ()
printMatch url = do
  m <- match url
  flip mapM_ (zip [1 ..] m) $ \ (n, s) -> do
    putStrLn $ "Set " ++ show n
    mapM_ print s
    putStrLn ""

