module Main where

type Team = String
data MatchResult = MatchResult{teamA :: Team, scoreA :: Int, teamB :: Team, scoreB :: Int}
type Points = Int
type Scores = [(Team, Points)]

plymouthArgyle :: Team
plymouthArgyle = "Plymouth Argyle"

liverpool :: Team
liverpool = "Liverpool"

celtic :: Team
celtic = "Celtic"

result1 :: MatchResult
result1 = MatchResult celtic 1 liverpool 0

result2 :: MatchResult
result2 = MatchResult plymouthArgyle 4 celtic 2

result3 :: MatchResult
result3 = MatchResult liverpool 3 plymouthArgyle 1

resultPoints :: Team -> MatchResult -> Points
resultPoints team (MatchResult ta sa tb sb)
    | team == winner = 2
    | sa == sb = 1
    | otherwise = 0
    where winner = if sa > sb then ta else tb

addResult :: Scores -> MatchResult -> Scores
addResult scores _ = scores -- TODO

main :: IO ()
main = do
    putStrLn "The current league table is..."
