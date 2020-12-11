{-|
Module:         Day11
Description:    <https://adventofcode.com/2020/day/11 Day 11: Seating System>
-}
module Day11 (day11a, day11b) where

import Data.Bool (bool)
import Data.Ix (inRange)
import qualified Data.Map as M (elems, fromDistinctAscList, keys, lookupIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (length, lines, unpack)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V (foldl', fromList, unsafeAccum, unsafeIndex)

parse :: Bool -> Text -> ([[Int]], Vector Bool)
parse far input = (adjs, V.fromList $ M.elems m) where
    rows = T.lines input
    maxY = length rows
    maxX = maximum $ 0 : map T.length rows
    limitVision
      | far = takeWhile $ inRange ((0, 0), (maxY - 1, maxX - 1))
      | otherwise = take 1
    m = M.fromDistinctAscList $ do
        (y, row) <- zip [0..] rows
        (x, c) <- zip [0..] $ T.unpack row
        (,) (y, x) <$> case c of
            '#' -> pure True
            'L' -> pure False
            _ -> mempty
    directions = filter (/= (0, 0)) $ (,) <$> [-1..1] <*> [-1..1]
    adjs = flip mapMaybe directions . look <$> M.keys m
    look (y, x) (dy, dx) = listToMaybe . mapMaybe (flip M.lookupIndex m) .
        limitVision $ zip [y + dy, y + 2 * dy..] [x + dx, x + 2 * dx..]

step :: Int -> [[Int]] -> Vector Bool -> Vector Bool
step d adjs m = V.unsafeAccum step' m $ zip [0..] adjs where
    step' False ixs = not $ any (V.unsafeIndex m) ixs
    step' True ixs = null . drop d . filter id $ V.unsafeIndex m <$> ixs

count :: Vector Bool -> Int
count = V.foldl' (flip $ bool id succ) 0

findDup :: (Eq a) => [a] -> Maybe a
findDup (x:xs@(y:_)) = if x == y then Just x else findDup xs
findDup _ = Nothing

day11a :: Text -> Maybe Int
day11a input = findDup . map count $ iterate (step 3 adjs) m0 where
    (adjs, m0) = parse False input

day11b :: Text -> Maybe Int
day11b input = findDup . map count $ iterate (step 4 adjs) m0 where
    (adjs, m0) = parse True input
