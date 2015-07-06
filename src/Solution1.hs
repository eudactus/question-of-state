module Solution1 where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)
type LeagueMap = M.Map Team Points
-- eliminate
type FootballScoring = (Int, Int) -> (Int, Int)
newtype ThreePointsForAWin a = ThreePointsForAWin { getPoints :: (a, a) }

class PairScoreable a where
    score :: a -> (Int, Int)

instance (Ord a) => PairScoreable (ThreePointsForAWin a) where
    score (ThreePointsForAWin (sa, sb))
        | sa > sb   = (3, 0)
        | sa < sb   = (0, 3)
        | otherwise = (1, 1)

-- eliminate
threePointsForAWin :: FootballScoring
threePointsForAWin (sa, sb)
    | sa > sb   = (3, 0)
    | sa < sb   = (0, 3)
    | otherwise = (1, 1)

leagueTable :: [MatchResult] -> [Score]
leagueTable results = reverse . sortBy (comparing snd) $ M.toList (calculateLeagueMap threePointsForAWin results)

calculateLeagueMap :: FootballScoring -> [MatchResult] -> LeagueMap
calculateLeagueMap scoring results = foldl updateLeague M.empty $ resultsScores scoring results

resultsScores :: FootballScoring -> [MatchResult] -> [Score]
resultsScores _ [] = []
resultsScores s (r:rs) = (singleResultScores s r) ++ (resultsScores s rs)

-- Introduce ThreePointsForAWin somehow
singleResultScores :: FootballScoring -> MatchResult -> [Score]
singleResultScores scoring (MatchResult ta sa tb sb) =
    let (ra, rb) = scoring (sa, sb)
        in [(ta, ra), (tb, rb)]

updateLeague :: LeagueMap -> Score -> LeagueMap
updateLeague leagueMap (team, points) =
    case M.lookup team leagueMap of Nothing -> M.insert team points leagueMap
                                    Just _ -> M.adjust (\oldPoints -> oldPoints + points) team leagueMap
