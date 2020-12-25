{-|
Module:         Day23
Description:    <https://adventofcode.com/2020/day/23 Day 23: Crab Cups>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Day23 (day23a, day23b) where

import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MV (length, unsafeNew, unsafeRead, unsafeWrite)
import Data.Char (digitToInt, isDigit)

step :: STVector s Int -> Int -> ST s Int
step vec x = do
    a <- MV.unsafeRead vec x
    b <- MV.unsafeRead vec a
    c <- MV.unsafeRead vec b
    y <- MV.unsafeRead vec c
    let pred' z = pred $ if z == 0 then MV.length vec else z
        t = until (`notElem` [a, b, c]) pred' $ pred' x
    u <- MV.unsafeRead vec t
    MV.unsafeWrite vec x y
    MV.unsafeWrite vec t a
    MV.unsafeWrite vec c u
    pure y

newVector :: [Int] -> ST s (STVector s Int)
newVector xs = do
    vec <- MV.unsafeNew $ length xs
    vec <$ zipWithM_ (MV.unsafeWrite vec) xs (drop 1 $ cycle xs)

day23a :: String -> Int
day23a input = runST $ do
    let nums@(x:_) = pred . digitToInt <$> filter isDigit input
    vec <- newVector nums
    foldM_ (const . step vec) x $ replicate 100 ()
    let f i acc = MV.unsafeRead vec i >>= \case
            0 -> pure acc
            j -> f j $! 10 * acc + j + 1
    f 0 0

day23b :: String -> Int
day23b input = runST $ do
    let nums@(x:_) = pred . digitToInt <$> filter isDigit input
    vec <- newVector $ nums ++ [9..999999]
    foldM_ (const . step vec) x $ replicate 10000000 ()
    y <- MV.unsafeRead vec 0
    z <- MV.unsafeRead vec y
    pure $ (y + 1) * (z + 1)
