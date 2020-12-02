module Day1Spec (spec) where

import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day1a (unlines ["1721", "979", "366", "299", "675", "1456"]) `shouldBe` Just 514579
    describe "part 2" $ do
        it "examples" $ do
            day1b (unlines ["1721", "979", "366", "299", "675", "1456"]) `shouldBe` Just 241861950
