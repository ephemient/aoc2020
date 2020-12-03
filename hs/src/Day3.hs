{-|
Module:         Day3
Description:    <https://adventofcode.com/2020/day/3 Day 3: Toboggan Trajectory>
-}
module Day3 (day3a, day3b) where

import Data.List.Split (chunksOf)

day3 :: Int -> [[Bool]] -> Int
day3 n = length . filter head . zipWith drop [0, n..] . map cycle

day3a :: String -> Int
day3a = day3 3 . map (map (== '#')) . lines

day3b :: String -> Int
day3b input = day3 1 g * day3 3 g * day3 5 g * day3 7 g * day3 1 g' where
    g = map (== '#') <$> lines input
    g' = head <$> chunksOf 2 g
