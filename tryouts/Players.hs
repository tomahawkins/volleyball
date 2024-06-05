module Players
  ( Player (..),
    allPlayers,
    Spot (..),
    allSpots,
  )
where

data Player
  = Aaliyah
  | Abby
  | AddieB
  | AddyD
  | Alisha
  | Amber
  | Anja
  | AnneKerry
  | Ari
  | AudreyS
  | AudrieO
  | Autumn
  | Aylie
  | BaileyB
  | BaileyU
  | Cassy
  | Chloe
  | Clea
  | Cora
  | Eden
  | Eliza
  | ElizaP
  | Etta
  | Faith
  | Finnley
  | GwenH
  | GwynS
  | Indie
  | Jayda
  | Kali
  | Kayla
  | Lael
  | Laine
  | Lauren
  | LaylaM
  | LaylaW
  | Lila
  | Lily
  | Lucy
  | Lulu
  | Maeve
  | Malaya
  | Mariam
  | Mary
  | Matilda
  | Merritt
  | Morgan
  | Neybi
  | Nyah
  | OliviaF
  | OliviaN
  | OliviaS
  | OliviaZ
  | Ophelia
  | Prudence
  | RileyE
  | RyleeD
  | RyleeParks
  | Sadie
  | Samantha
  | Selina
  | SofiaC
  | Sonoma
  | SophiaC
  | Story
  | Tierra
  | Violeta
  | Vivian
  | Wren
  | NolaD  -- Maybe Chloe?
  deriving (Show, Eq, Ord, Enum)

allPlayers :: [Player]
allPlayers = [Aaliyah .. Wren]

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
