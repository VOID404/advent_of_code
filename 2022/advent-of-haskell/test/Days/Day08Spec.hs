module Days.Day08Spec where

import           Data.List                (nub, permutations)
import           Day08
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Utils.Day08

spec :: Spec
spec = do
  describe "utils" $ do
    describe "repeating function" $ do
      prop "is id for n=0" $ do
        \f x -> funRepeat 0 (apply (f :: Fun Int Int)) x `shouldBe` (x :: Int)
      prop "repeats n times" $ do
        forAll (choose (1, 1000)) $
          \n x -> funRepeat n (+3) x `shouldBe` x + (n * 3)
    describe "rotating matrices" $
      it "produces 4 different values" $
        let rotated = map ($ permutations [1..4]) [id
                                 , rotate
                                 , rotate . rotate
                                 , rotate . rotate . rotate
                                 , rotate . rotate . rotate . rotate]
                  in (length . nub $ rotated) `shouldBe` 4

  input <- runIO $ readFile "./inputs/day08.txt"
  describe "basic" $ do
    it "basic sample" $
      Day08.basic Day08.sample `shouldBe` 21
    it "basic real" $
      Day08.basic input `shouldBe` 1543
    {-
    it "bonus sample" $
      Day08.bonus Day08.sample `shouldBe` 24933642
    it "bonus real" $
      Day08.bonus input `shouldBe` 1117448 -}
