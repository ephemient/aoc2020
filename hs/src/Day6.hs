{-|
Module:         Day6
Description:    <https://adventofcode.com/2020/day/6 Day 6: Custom Customs>
-}
module Day6 (day6a, day6b) where

import Data.Bits ((.&.), (.|.), popCount, shiftL)
import Data.Char (ord)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T (foldl', lines)

day6 :: Int -> (Int -> Int -> Int) -> Text -> Int
day6 initial merge = uncurry add . foldl' f (0, initial) . T.lines where
    add acc k = acc + popCount k
    f (acc, k) line = case T.foldl' g 0 line of
        0 -> (add acc k, initial)
        row -> (acc, merge k row)
    g a c = a .|. 1 `shiftL` (ord c .&. 31)

day6a :: Text -> Int
day6a = day6 0 (.|.)

day6b :: Text -> Int
day6b = day6 (-1) (.&.)
