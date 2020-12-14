{-# LANGUAGE OverloadedStrings #-}
module Day14Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day14 (day14a, day14b)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 = T.unlines
  [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  , "mem[8] = 11"
  , "mem[7] = 101"
  , "mem[8] = 0"
  ]
example2 = T.unlines
  [ "mask = 000000000000000000000000000000X1001X"
  , "mem[42] = 100"
  , "mask = 00000000000000000000000000000000X0XX"
  , "mem[26] = 1"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day14a example1 `shouldBe` Right 165
    describe "part 2" $ do
        it "examples" $ do
            day14b example2 `shouldBe` Right 208
