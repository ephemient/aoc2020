{-|
Module:         Day5
Description:    <https://adventofcode.com/2020/day/5 Day 5: Binary Boarding>
-}
module Day5 (day5a, day5b) where

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Bits (testBit)
import Data.Bool (bool)
import Data.Char (ord)
import Data.List ((\\))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
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
    nums <- mapM parse (lines input)
    (Min lo, Max hi) <- sconcat . fmap (Min &&& Max) <$> nonEmpty nums
    listToMaybe $ [lo..hi] \\ nums
