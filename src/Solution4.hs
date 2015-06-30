module Solution4 where

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
teamPosition tps = foldl addPoints startingPosition tps
    where theTeam = (team $ head tps)
          startingPosition = TeamPosition{teamPos = theTeam, pointsPos = 0, played = 0,won = 0, drawn = 0, lost = 0, goalsForPos = 0, goalsAgainstPos = 0, goalDifference = 0}
          addPoints pos pts = TeamPosition {    teamPos = (teamPos pos),
                                                pointsPos = (pointsPos pos) + (points pts),
                                                played = (played pos) + 1,
                                                won = (won pos) + if (goalsFor pts) > (goalsAgainst pts) then 1 else 0,
                                                drawn = (drawn pos) + if (goalsFor pts) == (goalsAgainst pts) then 1 else 0,
                                                lost = (lost pos) + if (goalsFor pts) < (goalsAgainst pts) then 1 else 0,
                                                goalsForPos = (goalsForPos pos) + (goalsFor pts),
                                                goalsAgainstPos = (goalsAgainstPos pos) + (goalsAgainst pts),
                                                goalDifference = (goalDifference pos) + (goalsFor pts) - (goalsAgainst pts)}

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

