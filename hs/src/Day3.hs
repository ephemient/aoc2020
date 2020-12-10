{-|
Module:         Day3
Description:    <https://adventofcode.com/2020/day/3 Day 3: Toboggan Trajectory>
-}
module Day3 (day3a, day3b) where

import Data.List (foldl')
import Data.Monoid (Sum(Sum))
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines)

day3a :: Text -> Int
day3a = length . filter ok . zip [0, 3..] . T.lines where
    ok (i, line) = T.index line (i `rem` T.length line) == '#'

day3b :: Text -> Int
day3b input = a * b * c * d * e where
    (Sum a, Sum b, Sum c, Sum d, Sum e) = foldl' (<>) mempty
      [ ( Sum $ count i
        , Sum . count $ 3 * i
        , Sum . count $ 5 * i
        , Sum . count $ 7 * i
        , case i `quotRem` 2 of
              (j, 0) -> Sum $ count j
              _ -> mempty
        )
      | (i, line) <- zip [0..] $ T.lines input
      , let count j = fromEnum $ T.index line (j `mod` T.length line) == '#'
      ]
