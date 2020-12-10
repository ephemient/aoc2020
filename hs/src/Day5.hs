{-|
Module:         Day5
Description:    <https://adventofcode.com/2020/day/5 Day 5: Binary Boarding>
-}
module Day5 (day5a, day5b) where

import Control.Monad ((>=>))
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (ord)
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T (foldl', lines)

parse :: Text -> Int
parse = flip shiftR 2 . T.foldl' f 0 where
    f acc c = acc `shiftL` 1 .|. complement (ord c) .&. 4

day5a :: Text -> Maybe Int
day5a = nonEmpty . map parse . T.lines >=> pure . maximum

day5b :: Text -> Maybe Int
day5b input = do
    let nums = sort $ parse <$> T.lines input
    _:|nums' <- nonEmpty nums
    succ . fst <$> find (\(l, r) -> l + 1 < r) (zip nums nums')
