{-# LANGUAGE TypeApplications #-}
module Day10Spec (spec) where

import Day10 (day10a, day10b)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: String
example1 = unlines $ show @Int <$> [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
example2 = unlines $ show @Int <$>
  [ 28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19
  , 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day10a example1 `shouldBe` 35
            day10a example2 `shouldBe` 220
    describe "part 2" $ do
        it "examples" $ do
            day10b example1 `shouldBe` 8
            day10b example2 `shouldBe` 19208
