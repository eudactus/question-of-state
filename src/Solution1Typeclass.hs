module Solution1Typeclass where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)
type LeagueMap = M.Map Team Points
newtype ThreePointsForAWin = ThreePointsForAWin { getPoints :: (Int, Int) }

class PairScoreable a where
    score :: a -> (Int, Int)

instance PairScoreable ThreePointsForAWin where
    score (ThreePointsForAWin (sa, sb))
        | sa > sb   = (3, 0)
        | sa < sb   = (0, 3)
        | otherwise = (1, 1)

leagueTable :: [MatchResult] -> [Score]
leagueTable results = reverse . sortBy (comparing snd) $ M.toList (calculateLeagueMap results)

calculateLeagueMap :: [MatchResult] -> LeagueMap
calculateLeagueMap results = foldl updateLeague M.empty $ resultsScores results

resultsScores :: [MatchResult] -> [Score]
resultsScores [] = []
resultsScores (r:rs) = (singleResultScores r) ++ (resultsScores rs)

singleResultScores :: MatchResult -> [Score]
singleResultScores (MatchResult ta sa tb sb) =
    let (ra, rb) = score $ ThreePointsForAWin (sa, sb)
        in [(ta, ra), (tb, rb)]

updateLeague :: LeagueMap -> Score -> LeagueMap
updateLeague leagueMap (team, points) =
    case M.lookup team leagueMap of Nothing -> M.insert team points leagueMap
                                    Just _ -> M.adjust (\oldPoints -> oldPoints + points) team leagueMap
