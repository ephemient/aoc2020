{-# LANGUAGE OverloadedStrings #-}
module Day25Spec (spec) where

import Day25 (day25)
import Test.Hspec (Spec, describe, it, shouldBe, it)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day25 "5764801\n17807724" `shouldBe` Just 14897079
