module Day3Spec (spec) where

import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day3a example `shouldBe` 7
    describe "part 2" $ do
        it "examples" $ do
            day3b example `shouldBe` 336
