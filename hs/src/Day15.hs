{-|
Module:         Day15
Description:    <https://adventofcode.com/2020/day/15 Day 15: Rambunctious Recitation>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day15 (day15) where

import Common (readEntire)
import qualified Data.IntMap as IntMap ((!?), fromList, insert)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T (splitOn, stripEnd)
import qualified Data.Text.Read as T (decimal)

day15 :: Int -> Text -> Either String Int
day15 n input = do
    nums <- mapM (readEntire T.decimal) . T.splitOn "," $ T.stripEnd input
    rhead :| rtail <- maybe (Left "empty") Right . nonEmpty $ reverse nums
    let start =
          ( length nums
          , maybe 0 succ $ elemIndex rhead rtail
          , IntMap.fromList $ zip nums [0..]
          )
        nums' = nums ++ map snd' (iterate f start)
    pure $ nums' !! (n - 1)
  where
    f (j, x, m) = (j + 1, maybe 0 (j -) $ m IntMap.!? x, IntMap.insert x j m)
    snd' (_, x, _) = x
