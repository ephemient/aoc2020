{-# LANGUAGE OverloadedStrings #-}
module Day24Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldBe, it)

example :: Text
example = T.unlines
  [ "sesenwnenenewseeswwswswwnenewsewsw"
  , "neeenesenwnwwswnenewnwwsewnenwseswesw"
  , "seswneswswsenwwnwse"
  , "nwnwneseeswswnenewneswwnewseswneseene"
  , "swweswneswnenwsewnwneneseenw"
  , "eesenwseswswnenwswnwnwsewwnwsene"
  , "sewnenenenesenwsewnenwwwse"
  , "wenwwweseeeweswwwnwwe"
  , "wsweesenenewnwwnwsenewsenwwsesesenwne"
  , "neeswseenwwswnwswswnw"
  , "nenwswwsewswnenenewsenwsenwnesesenew"
  , "enewnwewneswsewnwswenweswnenwsenwsw"
  , "sweneswneswneneenwnewenewwneswswnese"
  , "swwesenesewenwneswnwwneseswwne"
  , "enesenwswwswneneswsenwnewswseenwsese"
  , "wnwnesenesenenwwnenwsewesewsesesew"
  , "nenewswnwewswnenesenwnesewesw"
  , "eneswnwswnwsenenwnwnwwseeswneewsenese"
  , "neswnwewnwnwseenwseesewsenwsweewe"
  , "wseweeenwnesenwwwswnew"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day24a example `shouldBe` Right 10
    describe "part 2" $ do
        it "examples" $ do
            day24b example `shouldBe` Right 2208
