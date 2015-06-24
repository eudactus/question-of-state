module StatePatternLeague where

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Score = (Team, Points)

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
    | team == winner = 2
    | sa == sb = 1
    | otherwise = 0
    where winner = if sa > sb then ta else tb

addResult :: [Score] -> MatchResult -> [Score]
addResult scores _ = scores -- TODO

calculateScores :: [MatchResult] -> [Score]
calculateScores _ = []