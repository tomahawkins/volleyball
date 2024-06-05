module Inventory
  ( ankleBraces
  ) where

import Players
  ( Player (..),
    allPlayers,
  )

data Size = Xs | S | M deriving (Show, Eq)

ankleBraces :: [(Player, Size)]
ankleBraces =
  [ (GwenH, S),
    (Sadie, S),
    (Neybi, S),
    (Ophelia, S),
    (Cora, M),
    (Tierra, Xs),
    (Lauren, Xs),
    (Alisha, M),
    (Aylie, S),
    (Jayda, M),
    (RyleeParks, S),
    (Laine, S),
    (AnneKerry, S),
    (SophiaC, S),
    (AudrieO, M),
    (Amber, M),
    (Ari, S),
    (Morgan, S),
    (NolaD, S),
    (OliviaZ, S),
    (BaileyU, S),
    (Faith, S),
    (GwynS, S),
    (Finnley, M),
    (Samantha, S),
    (LaylaW, S),
    (Matilda, S),
    (AudreyS, S),
    (OliviaN, S),
    (RyleeD, Xs),
    (Aaliyah, S),
    (Malaya, S),
    (Maeve, S),
    (Lucy, S)
  ]
