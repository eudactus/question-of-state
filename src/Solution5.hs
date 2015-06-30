module Solution5 where

import qualified Data.List as L

type Team = String
type Points = Int
type Goals = Int

data MatchResult = MatchResult{teamA :: Team, scoreA :: Goals, teamB :: Team, scoreB :: Goals}
data TeamPoints = TeamPoints {team :: Team, teamGoalsFor :: Goals, teamGoalsAgainst :: Goals, teamPoints :: Points }
-- P/W/D/L/GF/GA/GD/PTS
data TeamPosition = TeamPosition {teamPos :: Team, played :: Int, won :: Int, drawn :: Int, lost :: Int,
                                    goalsFor :: Goals, goalsAgainst :: Goals, goalDifference :: Goals, points :: Points }
                                    deriving (Show)

type CompetitionResults  = [TeamPosition]

resultPoints :: Team -> MatchResult -> TeamPoints
resultPoints t (MatchResult ta sa tb sb) =
    TeamPoints { team = t,
                 teamGoalsFor = if t == ta then sa else sb,
                 teamGoalsAgainst = if t == ta then sb else sa,
                 teamPoints = points}
  where points | isWinner = 3 | isDraw  = 1 | otherwise = 0
        isWinner = (t == ta && sa > sb) || (t == tb && sb > sa)
        isDraw = sa == sb


teams :: [MatchResult] -> [Team]
teams results = L.nub(concat ([[(teamA m), (teamB m)] | m <- results]))

teamPosition :: [TeamPoints] -> TeamPosition
teamPosition tps = TeamPosition {   teamPos = (team $ head tps),
                                    points = sum [teamPoints tp | tp <- tps],
                                    played = length tps,
                                    won = length[tp | tp <- tps, (teamGoalsFor tp) > (teamGoalsAgainst tp)],
                                    drawn = length[tp | tp <- tps, (teamGoalsFor tp) == (teamGoalsAgainst tp)],
                                    lost = length[tp | tp <- tps, (teamGoalsFor tp) < (teamGoalsAgainst tp)],
                                    goalsFor = sum [teamGoalsFor tp | tp <- tps],
                                    goalsAgainst = sum [teamGoalsAgainst tp | tp <- tps],
                                    goalDifference = sum [(teamGoalsFor tp) - (teamGoalsAgainst tp) | tp <- tps]}

competitionResults :: [MatchResult] -> CompetitionResults
competitionResults results = [positionForTeam t | t <- teams results]
   where positionForTeam t = teamPosition [resultPoints t r | r <- resultsWhereTeamPlayed t]
         resultsWhereTeamPlayed t = [r | r <- results, t == (teamA r) || t == (teamB r)]

leagueTable :: CompetitionResults -> CompetitionResults
leagueTable tp = L.sortBy compareTeamPoints tp
  where compareTeamPoints t1 t2 = (points t2) `compare` (points t1)

