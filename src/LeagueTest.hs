module LeagueTest where

import Test.Hspec
import StatePatternLeague
import qualified Data.Map.Strict as M

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
    describe "calculateTable" $ do
        it "can score for one result" $ do
            let table = calculateTable [(MatchResult celtic 1 liverpool 0)]
            M.lookup celtic table `shouldBe` Just 2
            M.lookup liverpool table `shouldBe` Just 0
            M.lookup plymouthArgyle table `shouldBe` Nothing
        it "can score for two results" $ do
            let table = calculateTable [(MatchResult celtic 1 liverpool 0),(MatchResult celtic 1 plymouthArgyle 1)]
            M.lookup celtic table `shouldBe` Just 3
            M.lookup liverpool table `shouldBe` Just 0
            M.lookup plymouthArgyle table `shouldBe` Just 1