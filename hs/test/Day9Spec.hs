{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Day9Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Day9 (day9a, day9b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.pack . unlines $ show @Int <$>
  [ 35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102
  , 117, 150, 182, 127, 219, 299, 277, 309, 576
  ]


spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day9a 5 example  `shouldBe` Right (Just 127)
    describe "part 2" $ do
        it "examples" $ do
            day9b 5 example `shouldBe` Right (Just 62)
