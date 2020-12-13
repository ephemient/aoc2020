{-|
Module:         Day13
Description:    <https://adventofcode.com/2020/day/13 Day 13: Shuttle Search>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day13 (day13a, day13b) where

import Common (crt, readEntire)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.Read as T (decimal)

day13a :: Text -> Either String Int
day13a (T.lines -> [line1, line2]) = do
    n <- readEntire T.decimal line1
    let xs = rights $ readEntire T.decimal <$> T.splitOn "," line2
        (t, x) = minimum [((-n) `mod` p, p) | p <- xs]
    pure $ t * x
day13a _ = Left "no parse"

day13b :: Text -> Maybe Int
day13b (T.lines -> [_, input]) = Just . fromIntegral . fst $ foldr crt (0, 1)
  [ (t `mod` p, p :: Integer)
  | (t, Right p) <- zip [0, -1..] $ readEntire T.decimal <$> T.splitOn "," input
  ]
day13b _ = Nothing
