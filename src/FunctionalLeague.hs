module FunctionalLeague where

-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/containers/Data-Map-Strict.html
-- import qualified Data.Map.Strict as M
import qualified Data.List as L

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Goals, teamB :: Team, scoreB :: Goals}
type Points = Int
type Goals = Int
data TeamPoints = TeamPoints Team Points
data TeamMatchPoints = TeamMatchPoints Team [Points]

instance Eq TeamPoints where
  (TeamPoints _ p1) == (TeamPoints _ p2) = p1 == p2
instance Ord TeamPoints where
  (TeamPoints _ p1) `compare` (TeamPoints _ p2) = p1 `compare` p2

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
    | team == winner = 2
    | sa == sb = 1
    | otherwise = 0
    where winner = if sa > sb then ta else tb

--teamMatchPoints :: [MatchResult] -> [TeamMatchPoints]
--teamMatchPoints _ = []

--teamPoints :: [MatchResult] -> [TeamPoints]
--teamPoints mrs = map (\mp -> TeamPoints "x", 0 ) (teamMatchPoints mrs)

leagueTable :: [TeamPoints] -> [TeamPoints]
leagueTable tp = L.sort tp