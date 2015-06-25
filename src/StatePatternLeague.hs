module StatePatternLeague where

-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/containers/Data-Map-Strict.html
import qualified Data.Map.Strict as M

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)
type LeagueTable = M.Map Team Points

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
    | team == winner = 2
    | sa == sb = 1
    | otherwise = 0
    where winner = if sa > sb then ta else tb

addResult :: LeagueTable -> MatchResult -> LeagueTable
addResult table _ = table -- TODO

resultsScores :: [MatchResult] -> [Score]
resultsScores _ = [] -- TODO

-- fold over list Score where acc is LeagueTable
calculateTable :: [MatchResult] -> LeagueTable
calculateTable results = foldl applyScore M.empty $ resultsScores results

applyScore :: LeagueTable -> Score -> LeagueTable
applyScore _ _ = M.empty -- TODO