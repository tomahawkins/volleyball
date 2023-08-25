{-# LANGUAGE LambdaCase #-}

module RankingsByPlayers (rankingsByPlayers) where

import Players (Player (..), Spot (..))

rankingsByPlayers :: Player -> [Spot]
rankingsByPlayers = \case
  AbiR ->
    [ JvOutside,
      JvRightside,
      JvDefense,
      VarsityOutside,
      VarsityRightside,
      VarsityDefense,
      VarsitySetter,
      VarsityMiddle,
      CteamOutside,
      CteamRightside,
      CteamDefense
    ]
  AbiM ->
    [ JvSetter,
      JvOutside,
      JvDefense,
      CteamMiddle
    ]
  Alisha ->
    [ JvMiddle,
      JvDefense
    ]
  Angel ->
    [ CteamSetter,
      CteamRightside
    ]
  Avery ->
    [ VarsityDefense,
      VarsitySetter,
      JvDefense,
      JvSetter,
      CteamDefense,
      CteamSetter
    ]
  Bailey ->
    [ VarsityRightside,
      VarsityOutside,
      JvRightside,
      JvOutside,
      JvMiddle
    ]
  Cadence ->
    [ JvRightside,
      JvOutside,
      JvDefense,
      CteamOutside,
      CteamRightside,
      CteamDefense,
      CteamMiddle
    ]
  Caitlyn ->
    [ VarsityDefense,
      JvDefense,
      CteamDefense
    ]
  Cassy ->
    [ JvRightside,
      VarsityRightside,
      JvDefense,
      CteamSetter
    ]
  CatherineCh ->
    [ VarsityRightside,
      VarsityOutside,
      VarsityDefense,
      VarsityMiddle,
      JvOutside,
      JvRightside,
      JvMiddle,
      JvDefense
    ]
  Chayse ->
    [ VarsityOutside,
      VarsitySetter,
      VarsityRightside,
      VarsityMiddle,
      VarsityDefense
    ]
  Chris ->
    [ CteamSetter,
      CteamRightside,
      CteamMiddle,
      CteamOutside,
      CteamDefense
    ]
  Claire ->
    [ VarsityDefense,
      JvDefense,
      CteamDefense
    ]
  Clea ->
    [ JvSetter,
      CteamSetter,
      JvDefense,
      CteamDefense,
      CteamOutside,
      CteamRightside,
      CteamMiddle
    ]
  Clem ->
    [ JvMiddle,
      CteamMiddle,
      CteamOutside,
      CteamRightside,
      CteamSetter,
      CteamDefense
    ]
  Cora ->
    [ JvRightside,
      JvOutside
    ]
  Damaris ->
    [ JvRightside,
      CteamRightside,
      JvOutside,
      CteamOutside,
      CteamMiddle
    ]
  Eliza ->
    [ JvMiddle,
      CteamMiddle,
      CteamDefense,
      JvOutside,
      CteamOutside,
      CteamSetter,
      CteamRightside
    ]
  Etta ->
    [ VarsityDefense,
      JvDefense,
      CteamDefense,
      CteamMiddle
    ]
  Gwen ->
    [ JvSetter,
      CteamSetter,
      JvDefense,
      CteamDefense
    ]
  Hadley ->
    [ JvRightside,
      CteamRightside,
      CteamMiddle
    ]
  Indie ->
    [ JvOutside,
      JvDefense,
      JvMiddle,
      CteamOutside,
      JvRightside
    ]
  Isabel ->
    [ VarsityDefense,
      JvOutside,
      JvDefense,
      CteamOutside
    ]
  Jayda ->
    [ VarsityOutside,
      VarsityRightside,
      VarsityMiddle,
      VarsityDefense,
      JvOutside
    ]
  Josiah ->
    [ VarsityDefense,
      JvDefense,
      JvOutside
    ]
  Kayla ->
    [ JvDefense,
      JvRightside,
      CteamSetter
    ]
  Layla ->
    [ VarsityMiddle,
      VarsityOutside,
      JvMiddle,
      JvOutside,
      JvDefense
    ]
  Lily ->
    [ VarsityMiddle,
      VarsityRightside,
      JvDefense,
      VarsityOutside,
      JvOutside,
      JvMiddle
    ]
  Lulu ->
    [ JvMiddle,
      JvOutside,
      CteamMiddle,
      CteamRightside,
      CteamOutside,
      CteamSetter,
      CteamDefense
    ]
  Mariam ->
    [ JvSetter,
      CteamSetter,
      CteamOutside,
      CteamRightside
    ]
  Mary ->
    [ VarsityMiddle,
      VarsityOutside,
      VarsityRightside,
      VarsitySetter,
      JvMiddle
    ]
  Merritt ->
    [ VarsityMiddle,
      VarsityRightside,
      JvMiddle,
      JvOutside
    ]
  Meysha ->
    [ VarsitySetter,
      VarsityDefense,
      JvSetter,
      JvDefense,
      JvRightside
    ]
  Mia ->
    [ JvSetter,
      JvOutside,
      JvMiddle,
      CteamMiddle
    ]
  Neybi ->
    [ JvMiddle,
      JvDefense,
      CteamMiddle,
      JvRightside,
      CteamOutside,
      JvOutside
    ]
  Nyah ->
    [ JvDefense,
      CteamDefense
    ]
  Prudence ->
    [ JvOutside,
      JvMiddle,
      JvDefense,
      CteamOutside,
      JvRightside
    ]
  OliviaS ->
    [ VarsityMiddle,
      VarsityRightside,
      JvMiddle,
      JvRightside,
      JvOutside,
      CteamOutside,
      CteamDefense
    ]
  Ophelia ->
    [ VarsityDefense,
      VarsitySetter,
      JvDefense,
      JvSetter
    ]
  Sadie ->
    [ VarsitySetter,
      JvSetter,
      JvDefense,
      CteamSetter
    ]
  Samantha ->
    [ JvOutside,
      JvRightside,
      JvMiddle,
      CteamOutside,
      CteamRightside,
      CteamMiddle
    ]
  Sarah ->
    [ CteamOutside,
      CteamRightside,
      CteamDefense,
      JvOutside,
      JvRightside,
      JvDefense,
      CteamMiddle
    ]
  SofiaC ->
    [ JvOutside,
      VarsityOutside,
      VarsityRightside,
      JvRightside,
      JvSetter
    ]
  SophiaW ->
    [ JvOutside,
      CteamOutside,
      JvMiddle,
      CteamMiddle
    ]
  Tierra ->
    [ VarsityDefense,
      VarsityOutside,
      VarsityRightside,
      VarsityMiddle,
      JvDefense,
      JvOutside
    ]
  Violet ->
    [ JvRightside,
      JvOutside,
      CteamRightside,
      JvMiddle,
      JvDefense
    ]
  Wren ->
    [ VarsityDefense,
      JvDefense,
      JvRightside,
      JvOutside,
      CteamDefense
    ]
  _ -> []
