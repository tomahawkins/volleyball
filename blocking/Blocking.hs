module Main (main) where

import System.Environment
import Text.Printf

import Match
import Touches

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    "-t" : files -> do
      attacks <- mapM readFile files >>= return . concatMap parseTouches
      let percent a b = 100 * fromIntegral (length $ filter a $ filter b attacks) / fromIntegral (length $ filter b attacks)
          sampleSize a = length $ filter a attacks
          advantage (_, a) = a
          touched   (a, _) = a
          missed    (a, _) = not a
          report msg a b = printf "%s  %3.0f%%  (%d)\n" msg (percent a b :: Double) (sampleSize b)
      report "Advantage of all blocks    " advantage (const True) 
      report "Advantage of touched blocks" advantage touched
      report "Advantage of missed  blocks" advantage missed
    files -> do
      matches <- flip mapM files $ \ f -> readFile f >>= return . parseMatch
      putStrLn "Advantage analysis on individual matches:"
      putStrLn ""
      mapM_ reportMatch matches
      reportCombined matches

data Block = Block
  { team     :: Team
  , teamName :: String
  , blockers :: Int
  , gainAdvantage
  , wonPointInVolley
  , wonPointOnBlock
  , wonPointOnNextAttack :: Bool
  }

type Condition = Block -> Bool

parseBlocks :: Match -> [Block]
parseBlocks (Match teamA teamB sets) = concatMap blockOutcomes $ concat sets
  where
  blockOutcomes :: Volley -> [Block]
  blockOutcomes (Volley _ a winner) = f a
    where
    f a = case a of
      [] -> []
      [Attack side n] -> [Block
        { team                 = other side
        , teamName             = if side == A then teamB else teamA
        , blockers             = n
        , gainAdvantage        = xor side winner
        , wonPointInVolley     = xor side winner
        , wonPointOnBlock      = xor side winner
        , wonPointOnNextAttack = False
        }]
      Attack side n : a@(Attack side' _) : rest -> Block
        { team                 = other side
        , teamName             = if side == A then teamB else teamA
        , blockers             = n
        , gainAdvantage        = xor side side'
        , wonPointInVolley     = xor side winner
        , wonPointOnBlock      = False
        , wonPointOnNextAttack = null rest && xor side side' && xor side winner
        } : f (a : rest)
    other a = case a of
      A -> B
      B -> A
    xor a b = case (a, b) of
      (A, A) -> False
      (B, B) -> False
      _ -> True

occurs :: [Block] -> Condition -> Condition -> String
occurs blocks a b = printf "%3.0f%% (%4d / %4d)" percentage a' b'
  where
  a' = length $ filter a $ filter b blocks
  b' = length $            filter b blocks
  percentage :: Double
  percentage = 100 * fromIntegral a' / fromIntegral b'

reportMatch :: Match -> IO ()
reportMatch m@(Match teamA' teamB' _) = do
  printf "%s vs %s advantage gained:\n" teamA' teamB'
  putStrLn $ "  0 blockers:    " ++ teamA' ++ ": " ++ occurs blocks gainAdvantage (b0  `and'` teamA) ++ "    " ++ teamB' ++ ": " ++ occurs blocks gainAdvantage (b0  `and'` teamB)
  putStrLn $ "  1 blockers:    " ++ teamA' ++ ": " ++ occurs blocks gainAdvantage (b1  `and'` teamA) ++ "    " ++ teamB' ++ ": " ++ occurs blocks gainAdvantage (b1  `and'` teamB)
  putStrLn $ "  2 blockers:    " ++ teamA' ++ ": " ++ occurs blocks gainAdvantage (b2  `and'` teamA) ++ "    " ++ teamB' ++ ": " ++ occurs blocks gainAdvantage (b2  `and'` teamB)
  putStrLn $ "  3 blockers:    " ++ teamA' ++ ": " ++ occurs blocks gainAdvantage (b3  `and'` teamA) ++ "    " ++ teamB' ++ ": " ++ occurs blocks gainAdvantage (b3  `and'` teamB)
  putStrLn ""
  where
  blocks = parseBlocks m

reportTotals :: [Block] -> String -> Condition -> IO ()
reportTotals blocks msg condition = do
  putStrLn $ msg ++ ":"
  putStrLn $ "  0 blockers:    all teams: " ++ f b0 ++ "    Clarion: " ++ f b0'
  putStrLn $ "  1 blockers:    all teams: " ++ f b1 ++ "    Clarion: " ++ f b1'
  putStrLn $ "  2 blockers:    all teams: " ++ f b2 ++ "    Clarion: " ++ f b2'
  putStrLn $ "  3 blockers:    all teams: " ++ f b3 ++ "    Clarion: " ++ f b3'
  putStrLn ""
  where
  f = occurs blocks condition

reportCombined :: [Match] -> IO ()
reportCombined matches = do
  putStrLn ""
  reportTotals blocks "Advantage gained" gainAdvantage
  reportTotals blocks "Point won from block" wonPointOnBlock
  reportTotals blocks "Point won from attack after block" wonPointOnNextAttack
  reportTotals blocks "Point won from block or attack after block" (wonPointOnBlock `or'` wonPointOnNextAttack)
  where
  blocks = concatMap parseBlocks matches

b0 = (== 0) . blockers
b1 = (== 1) . blockers
b2 = (== 2) . blockers
b3 = (== 3) . blockers

b0' = b0 `and'` clarion
b1' = b1 `and'` clarion
b2' = b2 `and'` clarion
b3' = b3 `and'` clarion

teamA = (== A) . team
teamB = (== B) . team
clarion = (== "Clarion") . teamName

and' :: Condition -> Condition -> Condition
and' a b x = a x && b x

or' :: Condition -> Condition -> Condition
or' a b x = a x || b x

