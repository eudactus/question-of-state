module StatePatternLeague where

import qualified Data.Map.Strict as M

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)
type LeagueTable = M.Map Team Points

calculateTable :: [MatchResult] -> LeagueTable
calculateTable results = foldl updateTable M.empty $ resultsScores results

resultsScores :: [MatchResult] -> [Score]
resultsScores [] = []
resultsScores (r:rs) = (resultScores r) ++ (resultsScores rs)

resultScores :: MatchResult -> [Score]
resultScores (MatchResult ta sa tb sb)
    | sa > sb   = [(ta, 2), (tb, 0)]
    | sa < sb   = [(ta, 0), (tb, 2)]
    | otherwise = [(ta, 1), (tb, 1)]

updateTable :: LeagueTable -> Score -> LeagueTable
updateTable table (team, points) =
    case M.lookup team table of Nothing -> M.insert team points table
                                Just oldPoints -> M.adjust (\_ -> oldPoints + points) team table