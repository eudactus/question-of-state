module Test where

import Test.Hspec
import League

liverpool :: Team
liverpool = "Liverpool"

celtic :: Team
celtic = "Celtic"

main :: IO ()
main = hspec $ do
    describe "League table calculator" $ do
        it "scores 2 for a win" $ do
            resultPoints celtic (MatchResult celtic 1 liverpool 0) `shouldBe` 2
        it "scores 1 for a score draw" $ do
            resultPoints celtic (MatchResult celtic 3 liverpool 3) `shouldBe` 1
        it "scores 1 for a no score draw" $ do
            resultPoints celtic (MatchResult celtic 0 liverpool 0) `shouldBe` 1
        it "scores 0 for a lose" $ do
            resultPoints celtic (MatchResult celtic 0 liverpool 3) `shouldBe` 0
