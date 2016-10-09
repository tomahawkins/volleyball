module Roster
  ( Player (..)
  , roster
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup hiding (Row)

import PageCache
import Rosters
import Schools
import TagStruct hiding (getPage)
import Tables

data Player = Player
  { name     :: Maybe String
  , number   :: Maybe String
  , position :: Maybe String
  , height   :: Maybe String
  } deriving Show

roster :: String -> Int -> IO [Player]
roster school year = do
  page <- getPage $ url school "roster" $ Just id
  case find f . tables . parseTags $ page of
    Just a  -> return . map player . snd $ a
    Nothing -> return []
  where
  id = case lookup school rosters of
    Nothing    -> error $ "School not found: " ++ school
    Just years -> case lookup year years of
      Nothing -> error $ "Year not found: " ++ show year
      Just id -> id

  f :: Table -> Bool
  f a = case a of
    (("class", "default_dgrd roster_dgrd") : _, _) -> True
    _ -> False

  --f a = case a of
  --  TagStruct "table" (("class", "default_dgrd roster_dgrd") : _) _ -> True
  --  _ -> False

player :: Row -> Player
player (_, row) = Player (f name) (f num) (f pos) (f height)
  where
  f k
    | null r = Nothing
    | otherwise = Just $ intercalate " " r
    where
    r = mapMaybe k row

  name (p, a) = case (p, a) of
    ([("class", field)], a) | elem field ["roster_dgrd_full_name", "roster_dgrd_name"] -> Just $ concatMap text a
    _ -> Nothing

  num (p, a) = case (p, a) of
    ([("class", "roster_dgrd_no")], [TagData number]) -> Just $ filter (not . isSpace) number
    _ -> Nothing

  pos (p, a) = case (p, a) of
    ([("class", "roster_dgrd_rp_position_short")], [TagData p]) -> Just p
    _ -> Nothing
    
  height (p, a) = case (p, a) of
    ([("class", "roster_dgrd_height")], [TagStruct _ _ [TagData h]]) -> Just h
    _ -> Nothing
    
  text a = case a of
    TagStruct _ _ a -> concatMap text a
    TagData a -> filter (flip notElem "\r\n") a

