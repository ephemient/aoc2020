{-|
Module:         Day17
Description:    <https://adventofcode.com/2020/day/17 Day 17: Conway Cubes>
-}
{-# LANGUAGE TypeApplications #-}
module Day17 (day17a, day17b) where

import Control.Monad (replicateM)
import qualified Data.Map as Map (filterWithKey, fromListWith, keysSet)
import Data.Monoid (Sum(Sum))
import Data.Set (Set)
import qualified Data.Set as Set (elems, fromDistinctAscList, member)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

data Vec4 = Vec4 !Int !Int !Int !Int deriving (Eq, Ord)

parse :: Text -> Set Vec4
parse input = Set.fromDistinctAscList
  [ Vec4 x y 0 0
  | (x, line) <- zip [0..] $ T.lines input
  , (y, '#') <- zip [0..] $ T.unpack line
  ]

step :: Int -> Set Vec4 -> Set Vec4
step n s = Map.keysSet . Map.filterWithKey ok $ Map.fromListWith (<>)
  [ (Vec4 (x + dx) (y + dy) (z + dz) (w + dw), Sum @Int 1)
  | Vec4 x y z w <- Set.elems s
  , dx:dy:dz:dw:_ <- (++ repeat 0) <$> replicateM n [-1..1]
  , (dx, dy, dz, dw) /= (0, 0, 0, 0)
  ] where
    ok _ (Sum 3) = True
    ok p (Sum 2) = p `Set.member` s
    ok _ _ = False

day17a :: Text -> Int
day17a input = length $ iterate (step 3) (parse input) !! 6

day17b :: Text -> Int
day17b input = length $ iterate (step 4) (parse input) !! 6
