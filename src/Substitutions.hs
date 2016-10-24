module Substitutions
  ( substitutions
  ) where

import Match

-- | Determine substitiutions directions and add information if possible.
substitutions :: Team -> Name -> Set -> Set
substitutions team libero (Set events)
  | noNamedOuts team events = Set events
  | otherwise = Set $ flip map events $ \ a -> case a of
      Sub t a _ | t == team -> Sub t (everyOther a) (everyOther $ tail a)
      a -> a

noNamedOuts :: Team -> [Event] -> Bool
noNamedOuts team a = case a of
  [] -> False
  Sub t a _ : rest | t == team -> if odd $ length a then True else noNamedOuts team rest
  _ : rest -> noNamedOuts team rest

everyOther :: [a] -> [a]
everyOther a = case a of
  [] -> []
  [a] -> [a]
  a : _ : b -> a : everyOther b

