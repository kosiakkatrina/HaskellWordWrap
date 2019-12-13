module NumeralsSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Numerals" $ do
    it "returns I for 1" $ do
      convert 1 `shouldBe` "I"

    it "returns II for 2" $ do
      convert 2 `shouldBe` "II"

    it "returns III for 3" $ do
      convert 3 `shouldBe` "III"

    it "returns IV for 4" $ do
      convert 4 `shouldBe` "IV"

    it "returns V for 5" $ do
      convert 5 `shouldBe` "V"

    it "returns VI for 6" $ do
      convert 6 `shouldBe` "VI"

    it "returns VII for 7" $ do
      convert 7 `shouldBe` "VII"

    it "returns VIII for 8" $ do
      convert 8 `shouldBe` "VIII"

convert :: Int -> String
convert 4 = "IV"
convert n 
  | n>=5 = "V"++ (convert (n-5))
  | otherwise = take n (repeat 'I')


