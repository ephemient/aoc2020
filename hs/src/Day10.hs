{-|
Module:         Day10
Description:    <https://adventofcode.com/2020/day/10 Day 10: Adapter Array>
-}
{-# LANGUAGE TransformListComp, TypeApplications #-}
module Day10 (day10a, day10b) where

import Common (readEntire)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)

day10a :: Text -> Either String Int
day10a input = do
    nums <- sort @Int <$> mapM (readEntire T.decimal) (T.lines input)
    let diffs = zipWith (-) nums $ 0 : nums
        x = length $ filter (== 1) diffs
        y = length $ filter (== 3) diffs
    pure $ x * (y + 1)

day10b :: Text -> Either String Int
day10b input = do
    nums <- sort @Int <$> mapM (readEntire T.decimal) (T.lines input)
    pure . snd . head $ foldl f [(0, 1)] nums
  where f k n = (n, sum [w | (m, w) <- k, then takeWhile by m + 3 >= n]) : k
