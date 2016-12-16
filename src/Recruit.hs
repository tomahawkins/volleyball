module Recruit (recruit) where

import Data.List
import Text.HTML.TagSoup
import Text.Printf

import PageCache

recruit :: IO ()
recruit = do
  schools <- schools "pa"
  players <- flip mapM schools $ \ school -> do
    putStrLn school
    p <- rosters school
    let p' = seniorYearPlayers school p
    mapM_ print p'
    return p'
  sequence_ [ printf "%-25s %-50s  http://www.maxpreps.com%s\r\n" name school url | (Player _ name _ url, school) <- concat players ]
  --mapM_ putStrLn a
  --rosters clarion

clarion = "clarion-area-bobcats-(clarion,pa)"

-- | All schools in a state.
schools :: String -> IO [String]
schools state = do
  a <- getPage ("http://www.maxpreps.com/search/school.aspx?gendersport=girls,volleyball&state=" ++ state) >>= return . parseTags
  return [ reverse $ drop 20 $ reverse $ drop 14 url | TagOpen "a" [("href", url)] <- a, isPrefixOf "/high-schools/" url, isSuffixOf "/volleyball/home.htm" url, notElem '[' url ]

-- | All rosters of a school.
rosters :: String -> IO [Player]
rosters school = do
  var <- getPage ("http://www.maxpreps.com/high-schools/" ++ school ++    "/volleyball/all_time_roster.htm") >>= return . parseTags
  jv  <- getPage ("http://www.maxpreps.com/high-schools/" ++ school ++ "/jv-volleyball/all_time_roster.htm") >>= return . parseTags

  --putStrLn "Var"
  --mapM_ print var
  --putStrLn "JV"
  --mapM_ print jv 

  --mapM_ print $ seniorYearPlayers $ parseRoster 0 $ var ++ jv
  return $ parseRoster 0 $ var ++ jv

data Player = Player Int String Year String deriving Show
data Year = Freshman | Sophmore | Junior | Senior | Other deriving Show

parseRoster :: Int -> [Tag String] -> [Player]
parseRoster year'' a = case a of
  [] -> []
  TagText a : rest | isPrefixOf " 20" a -> parseRoster (read $ tail a) rest
  TagOpen "a" [("href", url)] : TagText name : TagOpen "span" [("class", "class-year")] : TagText year' : rest -> Player year'' name (year year') url : parseRoster year'' rest
  _ : rest -> parseRoster year'' rest

year :: String -> Year
year a = case a of
  "Fr." -> Freshman
  "So." -> Sophmore
  "Jr." -> Junior
  "Sr." -> Senior
  _     -> Other

seniorYearPlayers :: String -> [Player] -> [(Player, String)]
seniorYearPlayers school players
  | null players2014 || length players2015 >= 4 = []
  | otherwise        = [ (p, school) | p@(Player 2015 name Senior _) <- players, notElem name players2014 ]
  where
  players2014 = [ name | Player 2014 name _ _ <- players ]
  players2015 = [ (p, school) | p@(Player 2015 name Senior _) <- players, notElem name players2014 ]




