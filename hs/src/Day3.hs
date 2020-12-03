{-|
Module:         Day3
Description:    <https://adventofcode.com/2020/day/3 Day 3: Toboggan Trajectory>
-}
module Day3 (day3a, day3b) where

import Data.List
import Data.Ratio

day3 :: [Ratio Int] -> String -> Int
day3 slopes input = product $ length . filter id <$> transpose
  [ [ denominator slope == 1 && line !! (numerator slope `rem` n) == '#'
    | slope <- map (i *) slopes
    ]
  | (i, line) <- zip [0..] $ lines input
  , let n = length line
  ]

day3a :: String -> Int
day3a = day3 [3]

day3b :: String -> Int
day3b = day3 [1, 3, 5, 7, 1 % 2]
