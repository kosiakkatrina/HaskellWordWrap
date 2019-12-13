module MumblingSpec where

import Test.Hspec
import Data.Char

spec :: Spec
spec = do
    describe "Mumbling" $ do
        it "returns A for a" $ do
            mumble "a" `shouldBe` "A"

        it "returns B for b" $ do
            mumble "b" `shouldBe` "B"

        
        it "returns A-Bb for ab" $ do
            mumble "ab" `shouldBe` "A-Bb"    

        it "returns B-Cc for bc" $ do
            mumble "bc" `shouldBe` "B-Cc"  

        it "returns A-Bb-Ccc for abc" $ do
            mumble "abc" `shouldBe` "A-Bb-Ccc"

        it "returns A-Bb-Ccc for aBc" $ do
            mumble "aBc" `shouldBe` "A-Bb-Ccc"

        it "returns '' for ''" $ do
            mumble "" `shouldBe` ""
    
        
mumble :: String -> String
mumble n 
    | length n >= 2 =  (mumble  (init n)) ++ "-" ++ [toUpper (last n)] ++ (replicate (length n-1) $ toLower (last n))
    | otherwise = map toUpper n 