{-|
Module:         Day15
Description:    <https://adventofcode.com/2020/day/15 Day 15: Rambunctious Recitation>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day15 (day15) where

import Common (readEntire)
import Control.Monad (foldM, zipWithM_)
import Control.Monad.ST (runST)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T (splitOn, stripEnd)
import qualified Data.Text.Read as T (decimal)
import qualified Data.Vector.Unboxed.Mutable as MV (new, read, write)

day15 :: Int -> Text -> Either String Int
day15 n input = do
    nums <- mapM (readEntire T.decimal) . T.splitOn "," $ T.stripEnd input
    let m = length nums
        top = maximum $ n : (succ <$> nums)
    pure $ if n <= length nums then nums !! (n - 1) else runST $ do
        seen <- MV.new top
        zipWithM_ (MV.write seen) nums [1..m - 1]
        let f x i = do
                j <- MV.read seen x
                let y = if j == 0 then 0 else i - j
                MV.write seen x i $> y
        foldM f (last nums) [m..n - 1]
