module Days.Day10Spec where

import           Day10
import           Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day10.txt"
  describe "basic" $ do
    it "sample" $
      basic largeSample `shouldBe` 13140
    it "real" $
      basic input `shouldBe` 13480
  
  describe "bonus" $ do
    it "large sample" $
      bonus largeSample `shouldBe` largeResult
    it "real" $
      bonus input `shouldBe` realResult

largeResult = 
  [ "##..##..##..##..##..##..##..##..##..##.."
  , "###...###...###...###...###...###...###."
  , "####....####....####....####....####...."
  , "#####.....#####.....#####.....#####....."
  , "######......######......######......####"
  , "#######.......#######.......#######....."
  ]

realResult =
  [ "####..##....##.###...##...##..####.#..#."
  , "#....#..#....#.#..#.#..#.#..#.#....#.#.."
  , "###..#.......#.###..#....#....###..##..."
  , "#....#.##....#.#..#.#.##.#....#....#.#.."
  , "#....#..#.#..#.#..#.#..#.#..#.#....#.#.."
  , "####..###..##..###...###..##..#....#..#."
  ]
