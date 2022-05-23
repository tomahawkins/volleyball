module Main
  ( main
  ) where


import           Text.Printf


type Mas = Double  -- The Misty May Awesomeness Scale


type PlayerEffort = Double  -- I.e. Interest Rate


compound :: [PlayerEffort] -> [(PlayerEffort, Mas)]
compound effort = compound' 1 effort


compound' :: Mas -> [PlayerEffort] -> [(PlayerEffort, Mas)]
compound' m e = case e of
  []                          -> []
  todaysEffort : futureEffort -> (todaysEffort, m') : compound' m' futureEffort
    where m' = m + todaysEffort


playerGrowth :: [(PlayerEffort, Mas)]
playerGrowth = compound $ repeat 0.01


playerGrowth1Vs2 :: [((PlayerEffort, Mas), (PlayerEffort, Mas))]
playerGrowth1Vs2 = zip (compound $ repeat 0.01) (compound $ repeat 0.02)


playerGrowthVaried
  :: [((PlayerEffort, Mas), (PlayerEffort, Mas), (PlayerEffort, Mas))]
playerGrowthVaried = zip3 (compound (replicate 50 0.01 <> repeat 0.02))  -- Slacks off in the beginning.
                          (compound (replicate 50 0.02 <> repeat 0.01))  -- Slacks off in the end.
                          (compound $ repeat 0.02)                       -- Gives it her all.


days :: [a] -> [(Int, a)]
days a = take 100 $ zip [1 ..] a


format :: (PlayerEffort, Mas) -> String
format (e, m) = printf "mas(%1.0f%%): %1.2f" (e * 100) m


main :: IO ()
main = do
  putStrLn $ unlines
    [ printf "day: %3i    %s" d (format m) | (d, m) <- days playerGrowth ]

  {-
  putStrLn $ unlines
    [ printf "day: %3i    %s    %s" d (format m1) (format m2)
    | (d, (m1, m2)) <- days playerGrowth1Vs2
    ]

  putStrLn $ unlines
    [ printf "day: %3i    %s    %s   %s" d (format m1) (format m2) (format m3)
    | (d, (m1, m2, m3)) <- days playerGrowthVaried
    ]
    -}

