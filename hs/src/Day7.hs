{-|
Module:         Day7
Description:    <https://adventofcode.com/2020/day/7 Day 7: Handy Haversacks>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day7 (day7a, day7b) where

import Common (readEntire)
import Data.List (unfoldr)
import qualified Data.HashMap.Strict as Map (fromListWith, lookupDefault)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set (fromList, size)
import Data.Text (Text)
import qualified Data.Text as T (lines, words)
import qualified Data.Text.Read as T (decimal)

parse :: Text -> [((Text, Text), [(Int, (Text, Text))])]
parse = mapMaybe (parseLine . T.words) . T.lines where
    parseLine (a:b:_:_:rest) = Just ((a, b), unfoldr parseItems rest)
    parseLine _ = Nothing
    parseItems ((readEntire T.decimal -> Right n):a:b:_:rest) =
        Just ((n, (a, b)), rest)
    parseItems _ = Nothing

goal :: (Text, Text)
goal = ("shiny", "gold")

day7a :: Text -> Int
day7a input = Set.size . Set.fromList $ expand goal where
    bags = Map.fromListWith (++)
        [(item, [key]) | (key, items) <- parse input , (_, item) <- items]
    expand item = items ++ concatMap expand items where
        items = Map.lookupDefault [] item bags

day7b :: Text -> Int
day7b input = sum $ expand 1 goal where
    bags = Map.fromListWith (++) $ parse input
    expand n item = do
        (m', item') <- Map.lookupDefault [] item bags
        let n' = n * m'
        n' : expand n' item'
