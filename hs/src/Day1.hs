{-|
Module:         Day1
Description:    <https://adventofcode.com/2020/day/1 Day 1: Report Repair>
-}
module Day1 (day1a, day1b) where

import qualified Data.IntSet as IntSet (elems, fromList, member, split)
import Data.Maybe (listToMaybe)

day1a :: String -> Maybe Int
day1a input = listToMaybe
  [ x * y
  | x <- IntSet.elems nums
  , let (_, tailSet) = IntSet.split x nums
        y = 2020 - x
  , y `IntSet.member` tailSet
  ] where nums = IntSet.fromList $ map read $ lines input

day1b :: String -> Maybe Int
day1b input = listToMaybe
  [ x * y * z
  | x <- IntSet.elems nums
  , let (_, tailSet) = IntSet.split x nums
        (midSet, _) = IntSet.split (2020 - 2 * x) tailSet
  , y <- IntSet.elems midSet
  , let (_, tailSet') = IntSet.split y midSet
        z = 2020 - x - y
  , z `IntSet.member` tailSet'
  ] where nums = IntSet.fromList $ map read $ lines input
