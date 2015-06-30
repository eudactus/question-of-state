module Solution1 where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)
type LeagueMap = M.Map Team Points

leagueTable :: [MatchResult] -> [Score]
leagueTable results = reverse . sortBy (comparing snd) $ M.toList (calculateLeagueMap results)

calculateLeagueMap :: [MatchResult] -> LeagueMap
calculateLeagueMap results = foldl updateLeague M.empty $ resultsScores results

resultsScores :: [MatchResult] -> [Score]
resultsScores [] = []
resultsScores (r:rs) = (resultScores r) ++ (resultsScores rs)

resultScores :: MatchResult -> [Score]
resultScores (MatchResult ta sa tb sb)
    | sa > sb   = [(ta, 3), (tb, 0)]
    | sa < sb   = [(ta, 0), (tb, 3)]
    | otherwise = [(ta, 1), (tb, 1)]

updateLeague :: LeagueMap -> Score -> LeagueMap
updateLeague leagueMap (team, points) =
    case M.lookup team leagueMap of Nothing -> M.insert team points leagueMap
                                    Just _ -> M.adjust (\oldPoints -> oldPoints + points) team leagueMap
