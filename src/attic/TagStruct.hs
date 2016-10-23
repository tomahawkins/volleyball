module TagStruct
  ( TagStruct (..)
  , Params
  , getPage
  , tagStructs
  ) where

import Data.Char
import Data.List
import Text.HTML.TagSoup

import qualified PageCache as C

getPage :: String -> IO [TagStruct]
getPage url = C.getPage url >>= return . removeWhitespace . tagStructs . parseTags

data TagStruct
  = TagStruct String Params [TagStruct]
  | TagData   String

instance Show TagStruct where
  show a = case a of
    TagData a -> show a
    TagStruct name params items -> "<" ++ name ++ " " ++ show params ++ ">\n"
      ++ unlines (map ("  " ++) (lines $ intercalate "\n" $ map show items))
      ++ "</" ++ name ++ ">"

type Params = [(String, String)]

tagStructs :: [Tag String] -> [TagStruct]
tagStructs = tagStructs' [] []

tagStructs' :: [TagStruct] -> [(String, Params, [TagStruct])] -> [Tag String] -> [TagStruct]
tagStructs' top stack a = case (stack, a) of
  (_, [])                  -> top ++ concat [ a | (_, _, a) <- stack ]    -- Drop open tags w/o closed.

  (_,  TagOpen n p : rest) -> tagStructs' top ((n, p, []) : stack) rest

  ([], TagText a   : rest) -> tagStructs' (top ++ [TagData a]) [] rest
  ([], TagClose _  : rest) -> tagStructs' top stack rest                  -- Drop unpaired closed tag.

  ((n, p, items) : [], TagClose n' : rest)
    | n == n'   -> tagStructs' (top ++ [TagStruct n p items]) [] rest
    | otherwise -> tagStructs' top ((n, p, items) : stack) rest   -- Drop unmatched closed tag.

  ((n, p, items) : stack, TagText a : rest) -> tagStructs' top ((n, p, items ++ [TagData a]) : stack) rest

  ((n0, p0, items0) : (n1, p1, items1) : stack, TagClose n' : rest)
    | n0 == n'  -> tagStructs' top ((n1, p1, items1 ++ [TagStruct n0 p0 items0]) : stack) rest
    | otherwise -> tagStructs' top ((n1, p1, items1 ++ items0) : stack) (TagClose n' : rest)   -- Drop unmatched open tag.

  (_ , _ : rest) -> tagStructs' top stack rest    -- Drop other tags.

removeWhitespace :: [TagStruct] -> [TagStruct]
removeWhitespace a = case a of
  TagData a : rest
    | all isSpace a -> removeWhitespace rest
    | otherwise     -> TagData a : removeWhitespace rest
  TagStruct a b c : rest -> TagStruct a b (removeWhitespace c) : removeWhitespace rest
  [] -> []

