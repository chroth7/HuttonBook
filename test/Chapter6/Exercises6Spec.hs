module Chapter6.Exercises6Spec
  ( spec
  ) where

import           Chapter6.Exercises6
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "factorial" $ do
    it "n = 3" $
      factorial 3 `shouldBe` 6

    it "n = -3" $
      factorial (-3) `shouldBe` 0

  describe "sumdown" $ do
    it "n = 3" $
      sumdown 3 `shouldBe` 6

    it "n = -3" $
      sumdown (-3) `shouldBe` 0

  describe "^^^" $ do
    it "2 ^^^ 3" $
      2 ^^^ 3 `shouldBe` 8

    it "2 (^^^)2 (^^^) -3" $
      2 ^^^ (-3) `shouldBe` 0

  describe "euclid" $ do
    it "6 27" $
      euclid 6 27 `shouldBe` 3

    it "27 6" $
      euclid 27 6 `shouldBe` 3

    it "27 0" $
      euclid 27 0 `shouldBe` 0

    it "27 0" $
      euclid 27 0 `shouldBe` 0
