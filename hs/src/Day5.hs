{-|
Module:         Day5
Description:    <https://adventofcode.com/2020/day/5 Day 5: Binary Boarding>
-}
module Day5 (day5a, day5b) where

import Control.Monad ((>=>))
import Data.Bits (testBit)
import Data.Bool (bool)
import Data.Char (ord)
import Data.List (find, sort)
import Data.List.NonEmpty (nonEmpty)
import Numeric (readInt)

parse :: (Num a) => String -> Maybe a
parse a = case readInt 2 (`elem` "FBLR") f a of
    (n, []):_ -> Just n
    _ -> Nothing
  where f = bool 1 0 . flip testBit 2 . ord

day5a :: String -> Maybe Int
day5a = mapM parse . lines >=> nonEmpty >=> pure . maximum

day5b :: String -> Maybe Int
day5b input = do
    nums@(_:nums') <- sort <$> mapM parse (lines input)
    succ . fst <$> find (\(l, r) -> l + 1 < r) (zip nums nums')
