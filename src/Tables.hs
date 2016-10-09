module Tables
  ( Table
  , Row
  , Cell
  , tables
  ) where

import Text.HTML.TagSoup hiding (Row)

type Params = [(String, String)]
type Table = (Params, [Row])
type Row   = [Cell]
type Cell  = [Tag String]

-- Find all leaf tables in a page.  A faster implementation than TagStructing a whole page.
tables :: [Tag String] -> [Table]
tables a = case a of
  a@(TagOpen "table" params) : rest -> tables' params [a] rest
  _ : rest -> tables rest
  [] -> []

tables' :: Params -> [Tag String] -> [Tag String] -> [Table]
tables' params sofar a = case a of
  a@(TagClose "table") : rest -> (params, tableRows $ sofar ++ [a]) : tables rest
  a@(TagOpen "table" params) : rest -> tables' params [a] rest
  a : rest -> tables' params (sofar ++ [a]) rest
  [] -> error "Table tag not closed."

tableRows :: [Tag String] -> [Row]
tableRows a = case a of
  TagOpen "tr" _ : rest -> tableRows' [] rest
  _ : rest -> tableRows rest
  [] -> []

tableRows' :: [Cell] -> [Tag String] -> [Row]
tableRows' sofar a = case a of
  TagClose "tr"   : rest -> sofar : tableRows rest
  TagOpen  a _ : rest | elem a ["td", "th"] -> tableRows' (sofar ++ [d]) rest' where (d, rest') = tableCell [] rest
  _ : rest -> tableRows' sofar rest
  [] -> error "tableRows'"

tableCell :: [Tag String] -> [Tag String] -> ([Tag String], [Tag String])
tableCell sofar a = case a of
  TagClose a : rest | elem a ["td", "th"] -> (sofar, rest)
  a : rest -> tableCell (sofar ++ [a]) rest
  [] -> error "tableCell"

