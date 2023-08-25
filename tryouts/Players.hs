module Players
  ( Player (..),
    allPlayers,
    Spot (..),
    allSpots,
  )
where

data Player
  = AbiR
  | AbiM
  | Alisha
  | Aliyah
  | Angel
  | Angela
  | Ashley
  | Avery
  | Bailey
  | Cadence
  | Caitlyn
  | Cassy
  | CatherineCa
  | CatherineCh
  | Chayse
  | Chris
  | Claire
  | Clea
  | Clem
  | Cora
  | Damaris
  | Eliza
  | Etta
  | Fiona
  | Gwen
  | Hadley
  | Indie
  | Isabel
  | Jayda
  | Josiah
  | Kayla
  | Kaylee
  | Layla
  | Lila
  | Lily
  | Lulu
  | Maddy
  | Maiya
  | Mariam
  | Mary
  | Maya
  | Merritt
  | Meysha
  | Mia
  | Nadiya
  | Neybi
  | Nika
  | Nyah
  | OliviaE
  | OliviaS
  | Ophelia
  | Prudence
  | Ruby
  | Rylie
  | Sadie
  | Samantha
  | Sarah
  | SofiaC
  | SophiaW
  | Stephanie
  | Tierra
  | Vera
  | Violet
  | Wren
  deriving (Show, Eq, Ord, Enum)

allPlayers :: [Player]
allPlayers = [AbiR .. Wren]

data Spot
  = VarsitySetter
  | VarsityOutside
  | VarsityRightside
  | VarsityMiddle
  | VarsityDefense
  | JvSetter
  | JvOutside
  | JvRightside
  | JvMiddle
  | JvDefense
  | CteamSetter
  | CteamOutside
  | CteamRightside
  | CteamMiddle
  | CteamDefense
  deriving (Show, Eq, Ord, Enum)

allSpots :: [Spot]
allSpots = [VarsitySetter .. CteamDefense]
