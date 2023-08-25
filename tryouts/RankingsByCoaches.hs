module RankingsByCoaches
  ( pins,
    middles,
    defense,
    setters,
  )
where

import Players (Player (..))

showRankings :: Bool
showRankings = True

showIt :: [Player] -> [Player]
showIt p
  | showRankings = p
  | otherwise = []

pins :: [Player]
pins =
  showIt
    [ Chayse,
      Jayda,
      CatherineCh,
      Merritt,
      Lily,
      AbiR,
      Mary,
      OliviaS,
      Tierra,
      Indie,
      Bailey,
      Prudence,
      Layla,
      Cora,
      Wren,
      SofiaC,
      Cassy,
      Cadence,
      Eliza,
      Kayla,
      AbiM,
      Hadley,
      Violet,
      Lulu,
      Sarah,
      Neybi,
      Damaris
    ]

{- Unrated pins.
Angel
Chris
Clea
Clem
Isabel
Josiah
Mariam
Meysha
Mia
Samantha
SophiaW
-}

middles :: [Player]
middles =
  showIt
    [ Mary,
      Merritt,
      OliviaS,
      Lily,
      Layla,
      Prudence,
      Alisha,
      Eliza,
      Clem,
      Cadence,
      Neybi,
      Hadley,
      AbiM,
      Sarah,
      Lulu
    ]

{- Unrated middles.
AbiR
Bailey
CatherineCh
Chayse
Chris
Clea
Damaris
Etta
Indie
Jayda
Mia
Samantha
SophiaW
Tierra
Violet
-}

defense :: [Player]
defense =
  showIt
    [ Ophelia,
      Tierra,
      Josiah,
      Indie,
      Lily,
      Stephanie,
      Wren,
      Prudence,
      Cassy,
      Claire,
      AbiR,
      Sadie,
      Avery,
      Isabel,
      Violet,
      Sarah
    ]

{- Unrated defense.
AbiM
Alisha
Cadence
Caitlyn
CatherineCh
Chayse
Chris
Clea
Clem
Eliza
Etta
Gwen
Jayda
Kayla
Layla
Lulu
Meysha
Neybi
Nyah
OliviaS
-}

setters :: [Player]
setters =
  showIt
    [ Meysha,
      Sadie,
      Gwen,
      Kayla,
      Cassy,
      Clea,
      SofiaC,
      Avery
    ]

{- Unrated setters.
AbiR
AbiM
Angel
Chayse
Chris
Clem
Eliza
Lulu
Mariam
Mary
Mia
Ophelia
-}
