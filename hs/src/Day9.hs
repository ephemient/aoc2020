{-|
Module:         Day9
Description:    <https://adventofcode.com/2020/day/9 Day 9: Encoding Error>
-}
{-# LANGUAGE NondecreasingIndentation #-}
module Day9 (day9a, day9b) where

import Common (readEntire)
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Data.Sequence (ViewL((:<)), (|>), empty, viewl)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)

day9a :: Int -> Text -> Either String (Maybe Int)
day9a n input = do
    nums <- mapM (readEntire T.decimal) $ T.lines input
    pure $ day9a' n nums

day9a' :: Int -> [Int] -> Maybe Int
day9a' n nums = listToMaybe $ snd <$> filter (not . ok) windows where
    windows = fmap head . splitAt n <$> tails nums
    ok (window, z) =
        or [x + y == z | x:xs <- tails $ filter (< z) window, y <- xs]

day9b :: Int -> Text -> Either String (Maybe Int)
day9b n input = do
    nums <- mapM (readEntire T.decimal) $ T.lines input
    pure $ do
    target <- day9a' n nums
    let loop acc range list
          | acc == target, length range > 1
          = Just $ minimum range + maximum range
          | acc < target, (x:list') <- list = loop (acc + x) (range |> x) list'
          | (x :< range') <- viewl range = loop (acc - x) range' list
          | otherwise = Nothing
    loop 0 empty nums
