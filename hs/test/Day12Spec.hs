{-# LANGUAGE OverloadedStrings #-}
module Day12Spec (spec) where

import Data.Text (Text)
import Day12 (day12a, day12b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = "F10\nN3\nF7\nR90\nF11"

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day12a example `shouldBe` Right 25
    describe "part 2" $ do
        it "examples" $ do
            day12b example `shouldBe` Right 286
