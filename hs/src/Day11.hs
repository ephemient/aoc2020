{-|
Module:         Day11
Description:    <https://adventofcode.com/2020/day/11 Day 11: Seating System>
-}
module Day11 (day11a, day11b) where

import Data.Ix (inRange)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import qualified Data.Map as M ((!?), elems, findWithDefault, mapWithKey, singleton)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

parse :: Text -> Maybe (((Int, Int), (Int, Int)), Map (Int, Int) Bool)
parse text = do
    ((Min y0, Min x0), (Max y1, Max x1), m) <- sconcat <$> nonEmpty
      [ ((Min y, Min x), (Max y, Max x), M.singleton (y, x) $ c == '#')
      | (y, line) <- zip [0..] $ T.lines text
      , (x, c) <- zip [0..] $ T.unpack line
      , c `elem` "L#"
      ]
    pure (((y0, x0), (y1, x1)), m)

step :: (Map (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> Bool) -> Int -> Map (Int, Int) Bool -> Map (Int, Int) Bool
step adj d m = M.mapWithKey f m where
    f (y, x) b = if b then n < d else n == 0 where
        n = length $ filter (adj m (y, x))
            [(dy, dx) | dy <- [-1..1], dx <- [-1..1], dy /= 0 || dx /= 0]

count :: Map a Bool -> Int
count = length . filter id . M.elems

findDup :: (Eq a) => [a] -> Maybe a
findDup (x:xs@(y:_)) = if x == y then Just x else findDup xs
findDup _ = Nothing

day11a :: Text -> Maybe Int
day11a input = do
    (_, m0) <- parse input
    let adj m (y, x) (dy, dx) = M.findWithDefault False (y + dy, x + dx) m
    findDup . map count $ iterate (step adj 4) m0

day11b :: Text -> Maybe Int
day11b input = do
    (bounds, m0) <- parse input
    let adj m (y, x) (dy, dx) = fromMaybe False . listToMaybe .
            mapMaybe (m M.!?) . takeWhile (inRange bounds) $
            zip [y + dy, y + 2 * dy..] [x + dx, x + 2 * dx..]
    findDup . map count $ iterate (step adj 5) m0
