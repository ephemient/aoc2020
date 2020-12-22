{-# LANGUAGE OverloadedStrings #-}
module Day22Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day22 (day22a)
import Test.Hspec (Spec, describe, it, shouldBe, it)

example :: Text
example = T.unlines
  [ "Player 1:", "9", "2", "6", "3", "1", ""
  , "Player 2:", "5", "8", "4", "7", "10"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day22a example `shouldBe` Right 306
