{-# LANGUAGE OverloadedStrings #-}
module Day5Spec (spec) where

import Day5 (day5a)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day5a "FBFBBFFRLR" `shouldBe` Just 357
            day5a "BFFFBBFRRR" `shouldBe` Just 567
            day5a "FFFBBBFRRR" `shouldBe` Just 119
            day5a "BBFFBBFRLL" `shouldBe` Just 820
            day5a "FBFBBFFRLR\nBFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL" `shouldBe`
                Just 820
