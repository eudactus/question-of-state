module Solution2Main where

import Solution2

plymouthArgyle :: Team
plymouthArgyle = "Plymouth Argyle"

liverpool :: Team
liverpool = "Liverpool"

celtic :: Team
celtic = "Celtic"

main :: IO ()
main = do
    let result1 = MatchResult celtic 1 liverpool 0
        result2 = MatchResult plymouthArgyle 4 celtic 2
        result3 = MatchResult liverpool 1 plymouthArgyle 3
        results = [result1, result2, result3]
        leaguePositions = leagueTable $ competitionResults results
    putStrLn ("The current league table is...\n" ++ (show leaguePositions))
