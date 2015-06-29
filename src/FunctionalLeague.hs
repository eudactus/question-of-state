module FunctionalLeague where

-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/containers/Data-Map-Strict.html
-- import qualified Data.Map.Strict as M
import qualified Data.List as L
import Control.Arrow ((&&&))
import Data.Function

type Team = String
type Points = Int
type Goals = Int
type CompetitionResults  = [TeamPoints]

data MatchResult = MatchResult{teamA :: Team, scoreA :: Goals, teamB :: Team, scoreB :: Goals}
data TeamMatchResults = TeamMatchResults Team [MatchResult]
data TeamMatchPoints = TeamMatchPoints Team [Points]
data TeamPoints = TeamPoints Team Points

instance Show TeamPoints where
  show (TeamPoints t p) = t ++ " : " ++ (show p)

-- instance Show CompetitionResults where
--  show cr = foldl (\acc r -> acc ++ r ++ "\n") "" cr

groupToMap :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupToMap f = map (f . head &&& id)
                   . L.groupBy ((==) `on` f)
                   . L.sortBy (compare `on` f)

forSameHomeTeam :: MatchResult -> MatchResult -> Bool
forSameHomeTeam a b = (teamA a) == (teamA b)

forSameAwayTeam :: MatchResult -> MatchResult -> Bool
forSameAwayTeam a b = (teamB a) == (teamB b)

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
    | team == winner = 3
    | sa == sb = 1
    | otherwise = 0 
    where winner = if sa > sb then ta else tb

teamMatchResults :: [MatchResult] -> [TeamMatchResults]
teamMatchResults results =
  let homeResults = L.sortBy (compare `on` fst) (groupToMap teamA results)
      awayResults = L.sortBy (compare `on` fst) (groupToMap teamB results)
      combinedResults = zipWith (\h a -> ((fst h), (snd h) ++ (snd a))) homeResults awayResults
  in map (\r -> TeamMatchResults (fst r) (snd r)) combinedResults

teamMatchPoints :: [TeamMatchResults] -> [TeamMatchPoints]
teamMatchPoints results = map resultsToPoints results
  where resultsToPoints (TeamMatchResults t mrs) = TeamMatchPoints t (map (resultPoints t) mrs)
  
teamPoints :: TeamMatchPoints -> TeamPoints
teamPoints (TeamMatchPoints team points) = TeamPoints team (sum points)

teamTotalPoints :: [TeamMatchPoints] -> [TeamPoints]
teamTotalPoints tmps = map teamPoints tmps

competitionResults :: [MatchResult] -> CompetitionResults
competitionResults = teamTotalPoints . teamMatchPoints . teamMatchResults

leagueTable :: CompetitionResults -> CompetitionResults
leagueTable tp = L.sortBy compareTeamPoints tp
  where compareTeamPoints (TeamPoints _ p1) (TeamPoints _ p2) = p2 `compare` p1

