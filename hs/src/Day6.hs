{-|
Module:         Day6
Description:    <https://adventofcode.com/2020/day/6 Day 6: Custom Customs>
-}
module Day6 (day6a, day6b) where

import Data.List (foldl1', nub)
import Data.List.Split (splitOn)
import Data.Set (empty, fromList, intersection, size)

day6a :: String -> Int
day6a = sum . map (length . nub . filter (/= '\n')) . splitOn "\n\n"

day6b :: String -> Int
day6b = sum . map (size . intersects . fmap fromList . lines) . splitOn "\n\n"
  where intersects [] = empty
        intersects sets = foldl1' intersection sets
