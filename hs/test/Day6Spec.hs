module Day6Spec (spec) where

import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
    ["abc", "", "a", "b", "c", "", "ab", "ac", "", "a", "a", "a", "a", "", "b"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day6a (unlines ["abcx", "abcy", "abcz"]) `shouldBe` 6
            day6a example `shouldBe` 11
    describe "part 2" $ do
        it "examples" $ do
            day6b example `shouldBe` 6
