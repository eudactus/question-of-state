module Solution3 where

import qualified Data.List as L

type Team = String
type Points = Int
type Goals = Int

data MatchResult = MatchResult{teamA :: Team, scoreA :: Goals, teamB :: Team, scoreB :: Goals}
data TeamMatchResults = TeamMatchResults Team [MatchResult]
data TeamMatchPoints = TeamMatchPoints Team [Points]
data TeamPoints = TeamPoints Team Points

type CompetitionResults  = [TeamPoints]

instance Show TeamPoints where
  show (TeamPoints t p) = t ++ " : " ++ (show p)

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
  | team == winner = 3
  | sa == sb = 1
  | otherwise = 0
  where winner = if sa > sb then ta else tb

teams :: [MatchResult] -> [Team]
teams mrs = L.nub(concat (map (\r -> [(teamA r), (teamB r)]) mrs))

pointsForTeams :: [Team] -> [MatchResult] -> CompetitionResults
pointsForTeams ts results = map pointsForTeam ts
  where pointsForTeam team = TeamPoints team (sum (map (resultPoints team) results))

competitionResults :: [MatchResult] -> CompetitionResults
competitionResults results = pointsForTeams (teams results) results

leagueTable :: CompetitionResults -> CompetitionResults
leagueTable tp = L.sortBy compareTeamPoints tp
  where compareTeamPoints (TeamPoints _ p1) (TeamPoints _ p2) = p2 `compare` p1
