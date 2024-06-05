{-# LANGUAGE LambdaCase #-}

module RankingsByPlayers (rankingsByPlayers) where

import Players (Player (..), Spot (..))

rankingsByPlayers :: Player -> [Spot]
rankingsByPlayers = \case
  Abby ->
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
  Alisha ->
    [ JvMiddle,
      JvDefense
    ]
  BaileyB ->
    [ VarsityRightside,
      VarsityOutside,
      JvRightside,
      JvOutside,
      JvMiddle
    ]
  Cassy ->
    [ JvRightside,
      VarsityRightside,
      JvDefense,
      CteamSetter
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
  Cora ->
    [ JvRightside,
      JvOutside
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
  GwenH ->
    [ JvSetter,
      CteamSetter,
      JvDefense,
      CteamDefense
    ]
  Indie ->
    [ JvOutside,
      JvDefense,
      JvMiddle,
      CteamOutside,
      JvRightside
    ]
  Jayda ->
    [ VarsityOutside,
      VarsityRightside,
      VarsityMiddle,
      VarsityDefense,
      JvOutside
    ]
  Kayla ->
    [ JvDefense,
      JvRightside,
      CteamSetter
    ]
  LaylaM ->
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
  SofiaC ->
    [ JvOutside,
      VarsityOutside,
      VarsityRightside,
      JvRightside,
      JvSetter
    ]
  Tierra ->
    [ VarsityDefense,
      VarsityOutside,
      VarsityRightside,
      VarsityMiddle,
      JvDefense,
      JvOutside
    ]
  Wren ->
    [ VarsityDefense,
      JvDefense,
      JvRightside,
      JvOutside,
      CteamDefense
    ]
  _ -> []
