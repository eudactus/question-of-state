module Solution1 where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)
type LeagueMap = M.Map Team Points
type Scoring = (Int, Int) -> (Int, Int)

leagueTable :: [MatchResult] -> [Score]
leagueTable results = reverse . sortBy (comparing snd) $ M.toList (calculateLeagueMap footballScoring results)

calculateLeagueMap :: Scoring -> [MatchResult] -> LeagueMap
calculateLeagueMap scoring results = foldl updateLeague M.empty $ resultsScores scoring results

resultsScores :: Scoring -> [MatchResult] -> [Score]
resultsScores _ [] = []
resultsScores s (r:rs) = (resultScores s r) ++ (resultsScores s rs)

footballScoring :: Scoring
footballScoring (sa, sb)
    | sa > sb   = (3, 0)
    | sa < sb   = (0, 3)
    | otherwise = (1, 1)

resultScores :: Scoring -> MatchResult -> [Score]
resultScores scoring (MatchResult ta sa tb sb) =
    let (ra, rb) = scoring (sa, sb)
        in [(ta, ra), (tb, rb)]

updateLeague :: LeagueMap -> Score -> LeagueMap
updateLeague leagueMap (team, points) =
    case M.lookup team leagueMap of Nothing -> M.insert team points leagueMap
                                    Just _ -> M.adjust (\oldPoints -> oldPoints + points) team leagueMap
