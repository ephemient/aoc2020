{-# LANGUAGE OverloadedStrings #-}
module Day13Spec (spec) where

import Day13 (day13a, day13b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day13a "939\n7,13,x,x,59,x,31,19" `shouldBe` Right 295
    describe "part 2" $ do
        it "examples" $ do
            day13b "\n7,13,x,x,59,x,31,19" `shouldBe` Just 1068781
            day13b "\n17,x,13,19" `shouldBe` Just 3417
            day13b "\n67,7,59,61" `shouldBe` Just 754018
            day13b "\n67,x,7,59,61" `shouldBe` Just 779210
            day13b "\n67,7,x,59,61" `shouldBe` Just 1261476
            day13b "\n1789,37,47,1889" `shouldBe` Just 1202161486
