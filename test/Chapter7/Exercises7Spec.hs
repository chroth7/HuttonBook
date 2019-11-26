module Chapter7.Exercises7Spec
  ( spec
  ) where

import           Chapter7.Exercises7
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "filtermap" $ do
    it "works in a simple case" $
      filtermap even (*2) [1..6] `shouldBe` [4,8,12]

    it "works in another simple case" $
      filtermap odd (*2) [1..6] `shouldBe` [2,6,10]

  describe "and" $ do
    it "case1" $
      all' even [1..6] `shouldBe` False

    it "case2" $
      all' even [2,4,6] `shouldBe` True

    it "case3" $
      all' even [] `shouldBe` True

  describe "any" $ do
    it "case1" $
      any' even [1..6] `shouldBe` True

    it "case2" $
      any' even [1,3,5] `shouldBe` False

    it "case3" $
      any' even [] `shouldBe` False

  describe "takewhile" $ do
    it "case1" $
      takeWhile' even [1..6] `shouldBe` []

    it "case2" $
      takeWhile' odd [1..6] `shouldBe` [1]

    it "case3" $
      takeWhile' odd [1,3,5] `shouldBe` [1,3,5]

  describe "dropwhile" $ do
    it "case1" $
      dropWhile' even [1..6] `shouldBe` [1..6]

    it "case2" $
      dropWhile' odd [1..6] `shouldBe` [2..6]

    it "case3" $
      dropWhile' odd [1,3,5] `shouldBe` []

  describe "map" $ do
    it "case1" $
      map' (*2) [1..3] `shouldBe` [2,4,6]

    it "empty" $
      map (*2) [] `shouldBe` []

  describe "filter" $ do
    it "case1" $
      filter' even [1..6] `shouldBe` [2,4,6]

    it "empty" $
      filter' even [1,3,5] `shouldBe` []
