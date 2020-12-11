{-# LANGUAGE OverloadedStrings #-}
module Day11Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day11 (day11a, day11b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day11a example `shouldBe` Just 37
    describe "part 2" $ do
        it "examples" $ do
            day11b example `shouldBe` Just 26
