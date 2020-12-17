{-# LANGUAGE OverloadedStrings #-}
module Day17Spec (spec) where

import Data.Text (Text)
import Day17 (day17a, day17b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = ".#.\n..#\n###"

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day17a example `shouldBe` 112
    describe "part 2" $ do
        it "examples" $ do
            day17b example `shouldBe` 848
