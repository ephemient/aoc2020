{-# LANGUAGE OverloadedStrings #-}
module Day16Spec (spec) where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day16 (day16a, day16b')
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 = T.unlines
  [ "class: 1-3 or 5-7"
  , "row: 6-11 or 33-44"
  , "seat: 13-40 or 45-50"
  , ""
  , "your ticket:"
  , "7,1,14"
  , ""
  , "nearby tickets:"
  , "7,3,47"
  , "40,4,50"
  , "55,2,20"
  , "38,6,12"
  ]
example2 = T.unlines
  [ "class: 0-1 or 4-19"
  , "row: 0-5 or 8-19"
  , "seat: 0-13 or 16-19"
  , ""
  , "your ticket:"
  , "11,12,13"
  , ""
  , "nearby tickets:"
  , "3,9,18"
  , "15,1,5"
  , "5,14,9"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day16a example1 `shouldBe` Right 71
    describe "part 2" $ do
        it "examples" $ do
            fmap sort (day16b' example2) `shouldBe`
                Right [("class", 12), ("row", 11), ("seat", 13)]
