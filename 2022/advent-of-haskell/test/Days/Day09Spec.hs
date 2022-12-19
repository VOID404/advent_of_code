module Days.Day09Spec where

import           Day09
import           Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day09.txt"
  describe "basic" $ do
    it "sample" $
      basic sample `shouldBe` 13
    it "real" $
      basic input `shouldBe` 6256

  describe "bonus" $ do
    it "sample" $
      bonus sample `shouldBe` 1
    it "large sample" $
      bonus largeSample `shouldBe` 36
    it "real" $
      bonus input `shouldBe` 2665
