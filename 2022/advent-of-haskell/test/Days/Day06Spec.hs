module Days.Day06Spec where

import Day06
import Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day06.txt"
  describe "basic" $ do
    it "sample" $
      map Day06.basic Day06.samples `shouldBe` Day06.results
    it "real" $
      Day06.basic input `shouldBe` 1876

  describe "bonus" $ do
    it "sample" $
      map Day06.bonus Day06.samples `shouldBe` Day06.resultsBonus
    it "real" $
      Day06.bonus input `shouldBe` 2202
