module Chapter4.Exercises4Spec
  ( spec
  ) where

import           Chapter4.Exercises4
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Halves" $ do
    it "Halves an even-number-length array" $
      halve [1..6] `shouldBe` ([1,2,3],[4,5,6])

    it "Halves an odd-number-length array" $
      halve [1..7] `shouldBe` ([1,2,3],[4,5,6,7])

    it "Example test in QuickCheck" $ property $
       \x -> (read . show) x == (x :: Int)

  describe "third1" $ do
    it "Short list yields head" $
      third1 [1..2] `shouldBe` 1

    it "Normal, long list" $
      third1 [1..20] `shouldBe` 3

    it "Short list yields head" $
      third2 [1..2] `shouldBe` 1

    it "Normal, long list" $
      third2 [1..20] `shouldBe` 3

    -- it "Short list yields head" $
    --   third3 [1..2] `shouldBe` 1

    -- it "Normal, long list" $
    --   third3 [1..20] `shouldBe` 3

  describe "safetailCE" $ do
    it "works for empty" $
      safetailCE (tail [1]) `shouldBe` []

    it "works for non-empty" $
      safetailCE [1..10] `shouldBe` [2..10]

  describe "safetailGE" $ do
    it "works for empty" $
      safetailGE (tail [1]) `shouldBe` []

    it "works for non-empty" $
      safetailGE [1..10] `shouldBe` [2..10]

  describe "safetailPM" $ do
    it "works for empty" $
      safetailPM (tail [1]) `shouldBe` []

    it "works for non-empty" $
      safetailPM [1..10] `shouldBe` [2..10]

  describe "Luhn" $ do
    it "works in example 1" $
      luhn 1 7 8 4 `shouldBe` True

    it "works in example 2" $
      luhn 4 7 8 3 `shouldBe` False

