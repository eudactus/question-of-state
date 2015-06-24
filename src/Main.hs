module Main where

import League

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
        result3 = MatchResult liverpool 3 plymouthArgyle 1
    putStrLn ("The current league table is...")
