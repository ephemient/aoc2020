{-|
Module:         Day10
Description:    <https://adventofcode.com/2020/day/10 Day 10: Adapter Array>
-}
{-# LANGUAGE TransformListComp, TypeApplications #-}
module Day10 (day10a, day10b) where

import Data.List (sort)

day10a :: String -> Int
day10a input = x * (y + 1) where
    nums = sort $ read @Int <$> lines input
    diffs = zipWith (-) nums $ 0 : nums
    x = length $ filter (== 1) diffs
    y = length $ filter (== 3) diffs

day10b :: String -> Int
day10b = snd . head . foldl f [(0, 1)] . sort . map (read @Int) . lines where
    f k n = (n, sum [w | (m, w) <- k, then takeWhile by m + 3 >= n]) : k
