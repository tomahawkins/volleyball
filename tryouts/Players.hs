{-# LANGUAGE LambdaCase #-}

module Players
  ( Player (..),
    allPlayers,
    Spot (..),
    allSpots,
    playerName,
    readPlayers,
    checkPlayers,
  )
where

import Data.List (sort, (\\))

data Player
  = Aaliyah
  | AbbyM
  | AbbyW
  | Addie
  | Addy
  | Alisha
  | Amber
  | Anja
  | AnneKerry
  | Ari
  | Audrey
  | Audriana
  | Autumn
  | Aylie
  | BaileyB
  | BaileyU
  | Brithany
  | Cassy
  | Chloe
  | Clea
  | Cora
  | Eden
  | ElizaN
  | ElizaP
  | Etta
  | Faith
  | Finnley
  | Gwen
  | Gwyn
  | Indie
  | Jada
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
  | Mikel
  | Morgan
  | Neybi
  | Nola
  | Nyah
  | OliviaF
  | OliviaN
  | OliviaS
  | OliviaZ
  | Ophelia
  | Pearl
  | Prudence
  | Ramona
  | Riley
  | RyleeF
  | RyleeD
  | Sadie
  | Sam
  | Selina
  | Sofia
  | Sonoma
  | Sophia
  | Story
  | Tierra
  | Violeta
  | Vivian
  | Wren
  deriving (Show, Eq, Ord, Enum)

allPlayers :: [Player]
allPlayers = [Aaliyah .. Wren]

playerName :: Player -> String
playerName = \case
  Aaliyah -> "Erickson, Aaliyah"
  AbbyM -> "Morell, Abigail"
  AbbyW -> "Webb, Abigail"
  Addie -> "Bonner, Addison"
  Addy -> "Dupre, Addyson"
  Alisha -> "Garcia, Alisha"
  Amber -> "Keay, Amber"
  Anja -> "Greene, Anja"
  AnneKerry -> "Smith, Anne-Kerry"
  Ari -> "Weedman, Aurelia"
  Audrey -> "Sorgen, Audrey"
  Audriana -> "Owens, Audriana"
  Autumn -> "Cannell, Autumn"
  Aylie -> "Hughes, Aylie"
  BaileyB -> "Burge, Bailey"
  BaileyU -> "Urdiales, Bailey"
  Brithany -> "Valerio, Brithany"
  Cassy -> "Berwick, Cassandra"
  Chloe -> "Coppin, Chloe"
  Clea -> "Cristofaro, Clea"
  Cora -> "Allen, Cora"
  Eden -> "Moore, Eden"
  ElizaN -> "Ragsdale, Eliza"
  ElizaP -> "Parker, Eliza"
  Etta -> "Williard, Etta"
  Faith -> "Hoiland, Faith"
  Finnley -> "Boling, Finnley"
  Gwen -> "Hayden, Guinevere"
  Gwyn -> "Sellinger, Gwyneth"
  Indie -> "Shaver, Indie"
  Jada -> "Hinderman, Jada"
  Jayda -> "Knutsen, Jayda"
  Kali -> "Boak, Kali"
  Kayla -> "Morell, Kayla"
  Lael -> "Rowerdink, Lael"
  Laine -> "Garat, Laine"
  Lauren -> "Johnston, Lauren"
  LaylaM -> "Montroy, Layla"
  LaylaW -> "Worline, Layla"
  Lila -> "Geracie, Lila"
  Lily -> "Jude, Lilyauna"
  Lucy -> "Borchert, Lucy"
  Lulu -> "Munro, Louise"
  Maeve -> "Cochran, Maeve"
  Malaya -> "Robles, Malaya"
  Mariam -> "Najar, Mariam"
  Mary -> "Lockhart, Mary"
  Matilda -> "Mead, Matilda"
  Merritt -> "Jensen, Merritt"
  Mikel -> "Grove, Mikel"
  Morgan -> "Brennan, Morgan"
  Neybi -> "Vasquez, Neybi"
  Nola -> "Renner, Nola"
  Nyah -> "Lockwood, Nyah"
  OliviaF -> "Favro, Olivia"
  OliviaN -> "Naylor, Olivia"
  OliviaS -> "Siemanowski, Olivia"
  OliviaZ -> "Zawoysky, Olivia"
  Ophelia -> "Van-Leeuwen, Ophelia"
  Pearl -> "Hanna, Pearl"
  Prudence -> "Van-Leeuwen, Prudence"
  Ramona -> "Muir, Ramona"
  Riley -> "Eaton, Riley"
  RyleeD -> "Dwyer, Rylee"
  RyleeF -> "Foote, Rylee"
  Sadie -> "Hanley, Sadie"
  Sam -> "Moore, Samantha"
  Selina -> "Perez-Baker, Selina"
  Sofia -> "Challa, Sofia"
  Sonoma -> "Smith, Sonoma"
  Sophia -> "Colina-Chacon, Sophia"
  Story -> "Rayne, Saturn"
  Tierra -> "Bonner, Tierra"
  Violeta -> "Martinez-Farias, Violeta"
  Vivian -> "Tobosa, Vivian"
  Wren -> "Katzenberg, Wren"

checkPlayers :: FilePath -> IO ()
checkPlayers roster = do
  p1 <- sort <$> readPlayers roster
  let p2 = sort $ playerName <$> allPlayers
  if p1 == p2
    then putStrLn "All players accounted for."
    else do
      putStrLn $ "Players in database but not in final-forms: " <> show (p2 \\ p1)
      putStrLn $ "Players in final-forms but not in database: " <> show (p1 \\ p2)

readPlayers :: FilePath -> IO [String]
readPlayers f = readPlayers' <$> readFile f

readPlayers' :: String -> [String]
readPlayers' c = f2 <$> tail (lines c)
  where
    f1 = \case
      ',' -> ' '
      ' ' -> '-'
      a -> a
    f2 l = lastName <> ", " <> firstName
      where
        l1 = words $ f1 <$> l
        firstName = tail $ init $ l1 !! 2
        lastName = tail $ init $ l1 !! 3

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
