module LeagueTest where

import Test.Hspec
import Solution1
import qualified Data.Map.Strict as M

liverpool :: Team
liverpool = "Liverpool"

celtic :: Team
celtic = "Celtic"

plymouthArgyle :: Team
plymouthArgyle = "Plymouth Argyle"

main :: IO ()
main = hspec $ do
    describe "calculateLeagueMap" $ do
        it "can score for one result" $ do
            let table = calculateLeagueMap [(MatchResult celtic 1 liverpool 0)]
            M.lookup celtic table `shouldBe` Just 2
            M.lookup liverpool table `shouldBe` Just 0
            M.lookup plymouthArgyle table `shouldBe` Nothing
        it "can score for two results" $ do
            let table = calculateLeagueMap [(MatchResult celtic 1 liverpool 0),(MatchResult celtic 1 plymouthArgyle 1)]
            M.lookup celtic table `shouldBe` Just 3
            M.lookup liverpool table `shouldBe` Just 0
            M.lookup plymouthArgyle table `shouldBe` Just 1
    describe "leagueTable" $ do
        it "given one result has two scores ordered by points" $ do
            leagueTable [(MatchResult celtic 1 liverpool 0)]
            `shouldBe` [(celtic, 2), (liverpool, 0)]
        it "given two results has scores ordered by points" $ do
            leagueTable [(MatchResult celtic 1 liverpool 0),(MatchResult celtic 1 plymouthArgyle 1)]
            `shouldBe` [(celtic, 3), (plymouthArgyle, 1), (liverpool, 0)]            
