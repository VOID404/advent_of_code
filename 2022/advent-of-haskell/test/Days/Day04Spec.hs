module Days.Day04Spec where

import Day04
import Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day04.txt"
  describe "basic" $ do
    it "sample" $
      Day04.basic Day04.sample `shouldBe` 2
    it "real" $
      Day04.basic input `shouldBe` 515

  describe "bonus" $ do
    it "sample" $
      Day04.bonus Day04.sample `shouldBe` 4
    it "real" $
      Day04.bonus input `shouldBe` 883
