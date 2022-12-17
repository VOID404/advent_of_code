module Days.Day05Spec where

import Day05
import Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day05.txt"
  describe "basic" $ do
    it "sample" $
      Day05.basic Day05.sample `shouldBe` "CMZ"
    it "real" $
      Day05.basic input `shouldBe` "FJSRQCFTN"

  describe "bonus" $ do
    it "sample" $
      Day05.bonus Day05.sample `shouldBe` "MCD"
    it "real" $
      Day05.bonus input `shouldBe` "CJVLJQPHS"
