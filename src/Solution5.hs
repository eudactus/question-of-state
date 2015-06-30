module Solution5 where

import qualified Data.List as L

type Team = String
type Points = Int
type Goals = Int

data MatchResult = MatchResult{teamA :: Team, scoreA :: Goals, teamB :: Team, scoreB :: Goals}
data TeamPoints = TeamPoints {team :: Team, goalsFor :: Goals, goalsAgainst :: Goals, points :: Points }
-- P/W/D/L/GF/GA/GD/PTS
data TeamPosition = TeamPosition {teamPos :: Team, played :: Int, won :: Int, drawn :: Int, lost :: Int,
                                    goalsForPos :: Goals, goalsAgainstPos :: Goals, goalDifference :: Goals, pointsPos :: Points }
                                    deriving (Show)

type CompetitionResults  = [TeamPosition]

resultPoints :: Team -> MatchResult -> TeamPoints
resultPoints t (MatchResult ta sa tb sb) =
    TeamPoints { team = t,
                 goalsFor = if t == ta then sa else sb,
                 goalsAgainst = if t == ta then sb else sa,
                 points = points}
  where points | isWinner = 3 | isDraw  = 1 | otherwise = 0
        isWinner = (t == ta && sa > sb) || (t == tb && sb > sa)
        isDraw = sa == sb


teams :: [MatchResult] -> [Team]
teams results = L.nub(concat ([[(teamA m), (teamB m)] | m <- results]))

teamPosition :: [TeamPoints] -> TeamPosition
teamPosition tps = TeamPosition {   teamPos = (team $ head tps),
                                    pointsPos = sum [points tp | tp <- tps],
                                    played = length tps,
                                    won = length[tp | tp <- tps, (goalsFor tp) > (goalsAgainst tp)],
                                    drawn = length[tp | tp <- tps, (goalsFor tp) == (goalsAgainst tp)],
                                    lost = length[tp | tp <- tps, (goalsFor tp) < (goalsAgainst tp)],
                                    goalsForPos = sum [goalsFor tp | tp <- tps],
                                    goalsAgainstPos = sum [goalsAgainst tp | tp <- tps],
                                    goalDifference = sum [(goalsFor tp) - (goalsAgainst tp) | tp <- tps]}

competitionResults :: [MatchResult] -> CompetitionResults
competitionResults results = [positionForTeam t | t <- teams results]
   where positionForTeam t = teamPosition [resultPoints t r | r <- resultsWhereTeamPlayed t]
         resultsWhereTeamPlayed t = [r | r <- results, t == (teamA r) || t == (teamB r)]

leagueTable :: CompetitionResults -> CompetitionResults
leagueTable tp = L.sortBy compareTeamPoints tp
  where compareTeamPoints t1 t2 = (pointsPos t2) `compare` (pointsPos t1)

--instance Show TeamPosition where
--  show p = (teamPos p) ++ " : " ++ (show $ won p) ++ (show $ drawn p) ++ (show $ lost p)
--        ++ (show $ goalsForPos p) ++ (show $ won p)

