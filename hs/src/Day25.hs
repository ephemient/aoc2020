{-|
Module:         Day25
Description:    <https://adventofcode.com/2020/day/25 Day 25: Combo Breaker>
-}
{-# LANGUAGE DataKinds, TypeApplications #-}
module Day25 (day25) where

import Common (readEntire)
import Data.Either (rights)
import Data.Finite (getFinite)
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T (words)
import qualified Data.Text.Read as T (decimal)

day25 :: Text -> Maybe Integer
day25 input = do
    [pub1, pub2] <- pure . rights $ readEntire T.decimal <$> T.words input
    e <- elemIndex pub2 $ iterate (7 *) 1
    pure . getFinite @20201227 $ pub1 ^ e
