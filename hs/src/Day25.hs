{-|
Module:         Day25
Description:    <https://adventofcode.com/2020/day/25 Day 25: Combo Breaker>
-}
module Day25 (day25) where

import Common (readEntire)
import Data.Either (rights)
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T (words)
import qualified Data.Text.Read as T (decimal)

day25 :: Text -> Maybe Int
day25 input = do
    [pub1, pub2] <- pure . rights $ readEntire T.decimal <$> T.words input
    e <- elemIndex pub2 $ iterate ((`mod` 20201227) . (7 *)) 1
    pure $ pub1 `powMod` e

powMod :: (Integral b, Integral e) => b -> e -> b
powMod _ 0 = 1
powMod b e = powMod' 1 b e where
    powMod' k _ 0 = k
    powMod' k b' e' = powMod' k' b'' e'' where
        (e'', r) = e' `divMod` 2
        k' = if r == 0 then k else k * b' `mod` 20201227
        b'' = b' * b' `mod` 20201227
