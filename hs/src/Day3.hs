{-|
Module:         Day3
Description:    <https://adventofcode.com/2020/day/3 Day 3: Toboggan Trajectory>
-}
module Day3 (day3a, day3b) where

import Data.List (foldl')
import Data.Monoid (Sum(Sum))
import qualified Data.Vector.Unboxed as V ((!), fromList, length)

day3a :: String -> Int
day3a = length . filter ok . zip [0, 3..] . lines
  where ok (i, line) = line !! (i `rem` length line) == '#'

day3b :: String -> Int
day3b input = a * b * c * d * e where
    (Sum a, Sum b, Sum c, Sum d, Sum e) = foldl' (<>) mempty
      [ ( count i
        , count $ 3 * i
        , count $ 5 * i
        , count $ 7 * i
        , case i `quotRem` 2 of
              (j, 0) -> count j
              _ -> mempty
        )
      | (i, line) <- zip [0..] $ lines input
      , let line' = V.fromList line
            count j = Sum $ fromEnum $ line' V.! (j `rem` V.length line') == '#'
      ]
