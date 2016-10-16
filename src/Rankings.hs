module Rankings
  ( rankings
  ) where

import Data.Function
import Data.List

import Match

rankings :: [[Match]] -> [(Team, Double)]
rankings teamMatches = reverse $ sortBy (compare `on` snd) $ map rankTeam teamMatches
  where
  allTeams = map team teamMatches

  rankTeam :: [Match] -> (Team, Double)
  rankTeam matches = (team matches, ranking)
    where
    points' = concatMap matchPoints matches
    myPoints = filter (== team matches) points'
    ranking = fromIntegral (length myPoints) / fromIntegral (length points')

    matchPoints :: Match -> [Team]
    matchPoints m@(Match _ sets)
      | all (flip elem allTeams) [teamA, teamB] = concatMap points sets
      | otherwise = []
      where
      (teamA, teamB) = teams m


