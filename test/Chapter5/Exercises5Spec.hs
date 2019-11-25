module Chapter5.Exercises5Spec
  ( spec
  ) where

import           Chapter5.Exercises5
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "sumSquare" $ do
    it "n = 3" $
      sumSquare 3 `shouldBe` 14

    it "n = 100" $
      sumSquare 100 `shouldBe` 338350

  describe "grid" $
    it "grid 1 2" $
      grid 1 2 `shouldBe` [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

  describe "replicate" $
    it "replicate 3 True" $
      replicate 3 True `shouldBe` [True,True,True]

  describe "pyths" $
    it "pyths 10" $
      pyths 10 `shouldBe` [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

  describe "factor" $
    it "some factorizations" $ do
      factor 7 `shouldBe` [1]
      factor 15 `shouldBe` [1,3,5]

  describe "perfect!" $
    it "works for 500" $
      perfects 500 `shouldBe` [6,28,496]

  describe "scalarproduct" $
    it "works in a simple case" $
      scalarproduct [1,2,3] [4,5,6] `shouldBe` 32
