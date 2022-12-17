module Days.Day07Spec where

import Day07
import Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day07.txt"
  describe "basic" $ do
    it "sample" $
      Day07.basic Day07.sample `shouldBe` 95437
    it "real" $
      Day07.basic input `shouldBe` 1543140

  describe "bonus" $ do
    it "sample" $
      Day07.bonus Day07.sample `shouldBe` 24933642
    it "real" $
      Day07.bonus input `shouldBe` 1117448

