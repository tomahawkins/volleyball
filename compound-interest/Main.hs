module Main
  ( main
  ) where


import           Text.Printf


type Mas = Double  -- The Misty May Awesomeness Scale


type PlayerEffort = Double  -- I.e. Interest Rate


compound :: Mas -> PlayerEffort -> [Mas]
compound amount rate = result
 where
  c a = a : c a' where a' = a + amount * rate
  result = c amount
  --result = amount : compound (amount + amount * rate) rate


playerGrowth :: [Mas]
playerGrowth = compound 1 0.01


playerGrowth1Vs2 :: [(Mas, Mas)]
playerGrowth1Vs2 = zip (compound 1 0.01) (compound 1 0.02)


playerGrowthDelayed :: [(Mas, Mas)]
playerGrowthDelayed = zip (firstHalf <> secondHalf) committedFromBeginning
 where
  firstHalf              = take 51 $ compound 1 0.01
  secondHalf             = compound (last firstHalf) 0.02
  committedFromBeginning = compound 1 0.02


main :: IO ()
main = do
  putStrLn $ unlines
    [ printf "day: %3i    mas: %1.2f" d m
    | (d, m) <- take 101 $ zip [0 :: Int ..] playerGrowth
    ]

  --putStrLn $ unlines
  --  [ printf "day: %3i    mas(%%1): %1.2f    mas(%%2): %1.2f " d m1 m2
  --  | (d, (m1, m2)) <- take 101 $ zip [0 :: Int ..] playerGrowth1Vs2
  --  ]

  --putStrLn $ unlines
  --  [ printf "day: %3i    mas(%%1 then %%2): %1.2f    mas(%%2): %1.2f " d m1 m2
  --  | (d, (m1, m2)) <- take 101 $ zip [0 :: Int ..] playerGrowthDelayed
  --  ]


