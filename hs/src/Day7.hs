{-|
Module:         Day7
Description:    <https://adventofcode.com/2020/day/7 Day 7: Handy Haversacks>
-}
{-# LANGUAGE ViewPatterns #-}
module Day7 (day7a, day7b) where

import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map (elems, findWithDefault, fromList)
import Data.Maybe (mapMaybe)

parse :: String -> Map (String, String) [(Int, (String, String))]
parse = Map.fromList . mapMaybe (parseLine . words) . lines where
    parseLine (a:b:_:_:rest) = Just ((a, b), unfoldr parseItems rest)
    parseLine _ = Nothing
    parseItems ((reads -> (n, ""):_):a:b:_:rest) = Just ((n, (a, b)), rest)
    parseItems _ = Nothing

day7a :: String -> Int
day7a input = length . filter id $ Map.elems hasGold where
    bags = parse input
    hasGold = any (isTransitiveGold . snd) <$> bags
    isTransitiveGold ("shiny", "gold") = True
    isTransitiveGold item = Map.findWithDefault False item hasGold

day7b :: String -> Int
day7b input = sum $ expand 1 ("shiny", "gold") where
    bags = parse input
    expand n item = do
        (m', item') <- Map.findWithDefault [] item bags
        let n' = n * m'
        n' : expand n' item'
