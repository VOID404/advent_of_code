module Days.Day02Spec where
  
import           Day02
import           Test.Hspec

spec :: Spec
spec = do
  input <- runIO $ readFile "./inputs/day02.txt"
  
  describe "basic" $ do
    it "sample" $
      Day02.basic Day02.sample `shouldBe` 15
    it "real" $
      Day02.basic input `shouldBe` 12855

  describe "bonus" $ do
    it "sample" $
      Day02.bonus Day02.sample `shouldBe` 12
    it "real" $
      Day02.bonus input `shouldBe` 13726
