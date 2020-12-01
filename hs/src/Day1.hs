{-|
Module:         Day1
Description:    <https://adventofcode.com/2020/day/1 Day 1: Report Repair>
-}
module Day1 (day1a, day1b) where

import Data.List (tails)
import Data.Maybe (listToMaybe)

day1a :: String -> Maybe Int
day1a input = listToMaybe
  [ x * y
  | x:xs <- tails nums
  , y <- xs
  , x + y == 2020
  ] where nums = map read $ lines input

day1b :: String -> Maybe Int
day1b input = listToMaybe
  [ x * y * z
  | x:xs <- tails nums
  , y:ys <- tails xs
  , z <- ys
  , x + y + z == 2020
  ] where nums = map read $ lines input
