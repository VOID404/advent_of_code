import           Day01
import           Day02
import           Day03
import           Day04
import           Day05
import           Day06
import           Day07
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day01" $ do
    input <- runIO $ readFile "./inputs/day01.txt"

    it "basic sample" $
      Day01.basic Day01.sample `shouldBe` 24000
    it "basic real" $
      Day01.basic input `shouldBe` 71780

    it "bonus sample" $
      Day01.bonus Day01.sample `shouldBe` 45000
    it "bonus real" $
      Day01.bonus input `shouldBe` 212489

  describe "day02" $ do
    input <- runIO $ readFile "./inputs/day02.txt"

    it "basic sample" $
      Day02.basic Day02.sample `shouldBe` 15
    it "basic real" $
      Day02.basic input `shouldBe` 12855

    it "bonus sample" $
      Day02.bonus Day02.sample `shouldBe` 12
    it "bonus real" $
      Day02.bonus input `shouldBe` 13726

  describe "day03" $ do
    input <- runIO $ readFile "./inputs/day03.txt"

    it "basic sample" $
      Day03.basic Day03.sample `shouldBe` 157
    it "basic real" $
      Day03.basic input `shouldBe` 7597

    it "bonus sample" $
      Day03.bonus Day03.sample `shouldBe` 70
    it "bonus real" $
      Day03.bonus input `shouldBe` 2607

  describe "day04" $ do
    input <- runIO $ readFile "./inputs/day04.txt"

    it "basic sample" $
      Day04.basic Day04.sample `shouldBe` 2
    it "basic real" $
      Day04.basic input `shouldBe` 515

    it "bonus sample" $
      Day04.bonus Day04.sample `shouldBe` 4
    it "bonus real" $
      Day04.bonus input `shouldBe` 883

  describe "day05" $ do
    input <- runIO $ readFile "./inputs/day05.txt"

    it "basic sample" $
      Day05.basic Day05.sample `shouldBe` "CMZ"
    it "basic real" $
      Day05.basic input `shouldBe` "FJSRQCFTN"

    it "bonus sample" $
      Day05.bonus Day05.sample `shouldBe` "MCD"
    it "bonus real" $
      Day05.bonus input `shouldBe` "CJVLJQPHS"

  describe "day06" $ do
    input <- runIO $ readFile "./inputs/day06.txt"

    it "basic sample" $
      map Day06.basic Day06.samples `shouldBe` Day06.results
    it "basic real" $
      Day06.basic input `shouldBe` 1876

    it "bonus sample" $
      map Day06.bonus Day06.samples `shouldBe` Day06.resultsBonus
    it "bonus real" $
      Day06.bonus input `shouldBe` 2202

  describe "day07" $ do
    input <- runIO $ readFile "./inputs/day07.txt"

    it "basic sample" $
      Day07.basic Day07.sample `shouldBe` 95437

    it "basic real" $
      Day07.basic input `shouldBe` 1543140

    it "bonus sample" $
      Day07.bonus Day07.sample `shouldBe` 24933642
    it "bonus real" $
      Day07.bonus input `shouldBe` 1117448
