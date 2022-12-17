module Days.Day03Spec where

import           Day03
import           Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day03.txt"
  describe "basic" $ do
    it "sample" $
      Day03.basic Day03.sample `shouldBe` 157
    it "real" $
      Day03.basic input `shouldBe` 7597

  describe "bonus" $ do
    it "sample" $
      Day03.bonus Day03.sample `shouldBe` 70
    it "real" $
      Day03.bonus input `shouldBe` 2607
