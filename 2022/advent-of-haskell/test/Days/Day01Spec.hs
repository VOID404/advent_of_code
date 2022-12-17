module Days.Day01Spec where

import           Data.List             (intercalate)
import           Day01
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Utils.Day01           (split)

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day01.txt"

  describe "utils" $ do
    prop "split is opposite to intersperse" $
      \x y -> intercalate [y] (split y x) `shouldBe` (x :: String)

  describe "basic" $ do
    it "sample" $
      Day01.basic Day01.sample `shouldBe` 24000
    it "real" $
      Day01.basic input `shouldBe` 71780

  describe "bonus" $ do
    it "sample" $
      Day01.bonus Day01.sample `shouldBe` 45000
    it "real" $
      Day01.bonus input `shouldBe` 212489
