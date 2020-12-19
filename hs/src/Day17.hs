{-|
Module:         Day17
Description:    <https://adventofcode.com/2020/day/17 Day 17: Conway Cubes>
-}
{-# LANGUAGE TypeApplications #-}
module Day17 (day17a, day17b) where

import Control.Monad (replicateM)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Int (Int8)
import qualified Data.IntMap as IntMap (filterWithKey, fromListWith, keysSet)
import Data.Monoid (Sum(Sum))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (elems, fromDistinctAscList, member, size)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

pack :: Int8 -> Int8 -> Int8 -> Int8 -> Int
pack x y z w =
    fromIntegral x `shiftL` 24 .&. 0xff000000 .|.
    fromIntegral y `shiftL` 16 .&. 0x00ff0000 .|.
    fromIntegral z `shiftL`  8 .&. 0x0000ff00 .|.
    fromIntegral w             .&. 0x000000ff

unpack :: Int -> (Int8, Int8, Int8, Int8)
unpack v =
  ( fromIntegral $ v `shiftR` 24
  , fromIntegral $ v `shiftR` 16
  , fromIntegral $ v `shiftR`  8
  , fromIntegral   v
  )

parse :: Text -> IntSet
parse input = IntSet.fromDistinctAscList
  [ pack x y 0 0
  | (x, line) <- zip [0..] $ T.lines input
  , (y, '#') <- zip [0..] $ T.unpack line
  ]

step :: Int -> IntSet -> IntSet
step n s = IntMap.keysSet . IntMap.filterWithKey ok $ IntMap.fromListWith (<>)
  [ (pack (x + dx) (y + dy) (z + dz) (w + dw), Sum @Int 1)
  | (x, y, z, w) <- unpack <$> IntSet.elems s
  , dx:dy:dz:dw:_ <- (++ repeat 0) <$> replicateM n [-1..1]
  , (dx, dy, dz, dw) /= (0, 0, 0, 0)
  ] where
    ok _ (Sum 3) = True
    ok p (Sum 2) = p `IntSet.member` s
    ok _ _ = False

day17a :: Text -> Int
day17a input = IntSet.size $ iterate (step 3) (parse input) !! 6

day17b :: Text -> Int
day17b input = IntSet.size $ iterate (step 4) (parse input) !! 6
