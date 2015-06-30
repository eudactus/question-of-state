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

--instance Show TeamPosition where
--  show p = (teamPos p) ++ " : " ++ (show $ won p) ++ (show $ drawn p) ++ (show $ lost p)
--        ++ (show $ goalsForPos p) ++ (show $ won p)

resultPoints :: Team -> MatchResult -> TeamPoints
resultPoints t mr
  | (isWinner t mr) = TeamPoints { team = t, points = 3}
  | (isDraw mr) = TeamPoints { team = t, points = 1}
  | otherwise = TeamPoints { team = t, points = 0}

isWinner :: Team -> MatchResult -> Bool
isWinner t (MatchResult ta sa tb sb) = (t == ta && sa > sb) || (t == tb && sb > sa)

isDraw :: MatchResult -> Bool
isDraw (MatchResult _ sa _ sb) = sa == sb

teams :: [MatchResult] -> [Team]
teams results = L.nub(concat ([[(teamA m), (teamB m)] | m <- results]))

teamPosition :: [TeamPoints] -> TeamPosition
teamPosition tps = foldl addPoints TeamPosition{pointsPos = 0, played = 0} tps
    where addPoints pos pts = TeamPosition { teamPos = (team pts),
                                                pointsPos = (pointsPos pos) + (points pts),
                                                played = (played pos) + 1,
                                                won = 0, drawn = 0, lost = 0,
                                                goalsForPos = 0, goalsAgainstPos = 0, goalDifference = 0}

competitionResults :: [MatchResult] -> CompetitionResults
competitionResults results = [positionForTeam t | t <- teams results]
   where positionForTeam t = teamPosition [resultPoints t r | r <- results, t == (teamA r) || t == (teamB r)]

leagueTable :: CompetitionResults -> CompetitionResults
leagueTable tp = L.sortBy compareTeamPoints tp
  where compareTeamPoints t1 t2 = (pointsPos t2) `compare` (pointsPos t1)
