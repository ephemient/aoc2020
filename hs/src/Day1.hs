{-|
Module:         Day1
Description:    <https://adventofcode.com/2020/day/1 Day 1: Report Repair>
-}
module Day1 (day1a, day1b) where

import Common (readEntire)
import qualified Data.IntSet as IntSet (elems, fromList, member, split)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)

day1a :: Text -> Either String (Maybe Int)
day1a input = do
    nums <- IntSet.fromList <$> mapM (readEntire T.decimal) (T.lines input)
    pure $ listToMaybe
      [ x * y
      | x <- IntSet.elems nums
      , let (_, tailSet) = IntSet.split x nums
            y = 2020 - x
      , y `IntSet.member` tailSet
      ]

day1b :: Text -> Either String (Maybe Int)
day1b input = do
    nums <- IntSet.fromList <$> mapM (readEntire T.decimal) (T.lines input)
    pure $ listToMaybe
      [ x * y * z
      | x <- IntSet.elems nums
      , let (_, tailSet) = IntSet.split x nums
            (midSet, _) = IntSet.split (2020 - 2 * x) tailSet
      , y <- IntSet.elems midSet
      , let (_, tailSet') = IntSet.split y midSet
            z = 2020 - x - y
      , z `IntSet.member` tailSet'
      ]
