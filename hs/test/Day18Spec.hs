{-# LANGUAGE OverloadedStrings #-}
module Day18Spec (spec) where

import Day18 (day18a, day18b)
import Test.Hspec (Spec, describe, it, shouldBe, it)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day18a "1 + 2 * 3 + 4 * 5 + 6" `shouldBe` Right 71
            day18a "1 + (2 * 3) + (4 * (5 + 6))" `shouldBe` Right 51
            day18a "2 * 3 + (4 * 5)" `shouldBe` Right 26
            day18a "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` Right 437
            day18a "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe`
                Right 12240
            day18a "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe`
                Right 13632
    describe "part 2" $ do
        it "examples" $ do
            day18b "1 + 2 * 3 + 4 * 5 + 6" `shouldBe` Right 231
            day18b "1 + (2 * 3) + (4 * (5 + 6))" `shouldBe` Right 51
            day18b "2 * 3 + (4 * 5)" `shouldBe` Right 46
            day18b "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` Right 1445
            day18b "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe`
                Right 669060
            day18b "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe`
                Right 23340
