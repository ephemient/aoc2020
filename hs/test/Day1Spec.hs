{-# LANGUAGE OverloadedStrings #-}
module Day1Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["1721", "979", "366", "299", "675", "1456"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day1a example `shouldBe` Right (Just 514579)
    describe "part 2" $ do
        it "examples" $ do
            day1b example `shouldBe` Right (Just 241861950)
