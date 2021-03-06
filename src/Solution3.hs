module Solution3 where

import qualified Data.List as L

type Team = String
type Points = Int
type Goals = Int

data MatchResult = MatchResult{teamA :: Team, scoreA :: Goals, teamB :: Team, scoreB :: Goals}
data TeamPoints = TeamPoints Team Points

type CompetitionResults  = [TeamPoints]

instance Show TeamPoints where
  show (TeamPoints t p) = t ++ " : " ++ (show p)

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
  | team == winner = 3
  | isADraw = 1
  | otherwise = 0
  where winner = if sa > sb then ta else tb
        isADraw = sa == sb

teams :: [MatchResult] -> [Team]
teams results = L.nub(concat ([[(teamA m), (teamB m)] | m <- results]))

competitionResults :: [MatchResult] -> CompetitionResults
competitionResults results = [pointsForTeam t | t <- teams results]
   where pointsForTeam team = TeamPoints team $ sum [resultPoints team r | r <- results]

leagueTable :: CompetitionResults -> CompetitionResults
leagueTable tp = L.sortBy compareTeamPoints tp
  where compareTeamPoints (TeamPoints _ p1) (TeamPoints _ p2) = p2 `compare` p1
