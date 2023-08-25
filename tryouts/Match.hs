module Match
  ( match,
  )
where

import Data.List (elemIndex, sortOn)

-- Matches applicants to available spots.
match ::
  (Ord spot, Ord applicant) =>
  [(spot, [applicant])] ->
  [(applicant, [spot])] ->
  [(spot, Maybe applicant)]
match rankingsBySpots rankingsByApplicants = placements
  where
    initialSpots = [(s, r, Nothing) | (s, r) <- rankingsBySpots]
    placements = sortOn fst [(s, p) | (s, _, p) <- matchFixPoint rankingsByApplicants initialSpots]

matchFixPoint ::
  (Ord spot, Ord applicant) =>
  [(applicant, [spot])] ->
  [(spot, [applicant], Maybe applicant)] ->
  [(spot, [applicant], Maybe applicant)]
matchFixPoint rankingsByApplicants a
  | a == next = a
  | otherwise = matchFixPoint rankingsByApplicants next
  where
    next = matchRound rankingsByApplicants a

matchRound ::
  (Ord spot, Ord applicant) =>
  [(applicant, [spot])] ->
  [(spot, [applicant], Maybe applicant)] ->
  [(spot, [applicant], Maybe applicant)]
matchRound rankingsByApplicants spots = foldr matchApplicant spots availableApplicants
  where
    placedApplicants = [a | (_, _, Just a) <- spots]
    availableApplicants = [a | a@(applicant, _) <- rankingsByApplicants, notElem applicant placedApplicants]

matchApplicant ::
  (Ord spot, Ord applicant) =>
  (applicant, [spot]) ->
  [(spot, [applicant], Maybe applicant)] ->
  [(spot, [applicant], Maybe applicant)]
matchApplicant a@(_, spotRankings) spots = matchApplicant' a $ sortOn spotsSortedOn spots
  where
    spotsSortedOn (spot, _, _) = case elemIndex spot spotRankings of
      Nothing -> length spotRankings
      Just i -> i

-- Assumes spots sorted by player preference.
matchApplicant' ::
  (Ord spot, Ord applicant) =>
  (applicant, [spot]) ->
  [(spot, [applicant], Maybe applicant)] ->
  [(spot, [applicant], Maybe applicant)]
matchApplicant' a@(applicant, spotRankings) spots = case spots of
  [] -> []
  s@(spot, applicantRankings, Nothing) : rest
    | notElem spot spotRankings -> s : rest
    | elem applicant applicantRankings -> (spot, applicantRankings, Just applicant) : rest
    | otherwise -> (spot, applicantRankings, Nothing) : matchApplicant' a rest
  s@(spot, applicantRankings, Just otherApplicant) : rest
    | notElem spot spotRankings -> s : rest
    | otherwise -> case (elemIndex applicant applicantRankings, elemIndex otherApplicant applicantRankings) of
        (Nothing, _) -> s : matchApplicant' a rest
        (Just _, Nothing) -> error "Shouldn't get here."
        (Just i, Just j)
          | i < j -> (spot, applicantRankings, Just applicant) : rest
          | otherwise -> s : matchApplicant' a rest
