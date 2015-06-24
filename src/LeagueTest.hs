module LeagueTest where

import Test.Hspec
import StatePatternLeague

liverpool :: Team
liverpool = "Liverpool"

celtic :: Team
celtic = "Celtic"

plymouthArgyle :: Team
plymouthArgyle = "Plymouth Argyle"

main :: IO ()
main = hspec $ do
    describe "resultPoints" $ do
        it "scores 2 for a win" $ do
            resultPoints celtic (MatchResult celtic 1 liverpool 0) `shouldBe` 2
        it "scores 1 for a score draw" $ do
            resultPoints celtic (MatchResult celtic 3 liverpool 3) `shouldBe` 1
        it "scores 1 for a no score draw" $ do
            resultPoints celtic (MatchResult celtic 0 liverpool 0) `shouldBe` 1
        it "scores 0 for a lose" $ do
            resultPoints celtic (MatchResult celtic 0 liverpool 3) `shouldBe` 0
    describe "calculateScores" $ do
        it "can score for one result" $ do
            calculateScores [
                (MatchResult celtic 1 liverpool 0)]
                `shouldBe`
                [(celtic, 2), (liverpool, 0)]
        it "can score for two results" $ do
            calculateScores [
                (MatchResult celtic 1 liverpool 0),
                (MatchResult celtic 1 plymouthArgyle 1)]
                `shouldBe`
                [(celtic, 3), (liverpool, 0), (plymouthArgyle, 1)]            
