module Rankings
  ( rankings
  ) where

import Data.Function
import Data.List

import Match

rankings :: [Season] -> [(Team, Double)]
rankings seasons = reverse $ sortBy (compare `on` snd) $ map rankTeam seasons
  where
  allTeams = [ t | Season t _ <- seasons ]

  rankTeam :: Season -> (Team, Double)
  rankTeam (Season team matches) = (team, ranking)
    where
    points' = concatMap matchPoints matches
    myPoints = filter (== team) points'
    ranking = fromIntegral (length myPoints) / fromIntegral (length points')

    matchPoints :: Match -> [Team]
    matchPoints m@(Match _ sets)
      | all (flip elem allTeams) [teamA, teamB] = concatMap points sets
      | otherwise = []
      where
      (teamA, teamB) = teams m


