{-|
Module:         Day25
Description:    <https://adventofcode.com/2020/day/25 Day 25: Combo Breaker>
-}
module Day25 (day25) where

import Common (readEntire)
import Control.Monad (guard)
import Data.Either (rights)
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T (words)
import qualified Data.Text.Read as T (decimal)
import Debug.Trace (traceShowM)

(*%), (/%) :: (Integral a) => a -> a -> a
a *% b = a * b `mod` 20201227
a /% b = a * recipMod b `mod` 20201227
infixl 7 *%, /%

(^%) :: (Integral a) => a -> Int -> a
_ ^% 0 = 1
a ^% e = case e `divMod` 2 of
    (e', 0) -> (a * a) ^% e'
    (e', _) -> a *% (a * a) ^% e'
infixr 8 ^%

recipMod :: (Integral a) => a -> a
recipMod a = a ^% 20201225

elemIndexAll :: (Eq a) => [a] -> [a] -> Maybe [Int]
elemIndexAll = elemIndices' 0 . map Left where
    elemIndices' _ k _ | Right k' <- sequenceA k = Just k'
    elemIndices' _ _ [] = Nothing
    elemIndices' n k (x:xs) = elemIndices' (n + 1) (check n x <$> k) xs
    check n x (Left x') | x == x' = Right n
    check _ _ k = k

day25 :: Text -> Maybe Int
day25 input = do
    -- pub1 ≅ 7 ^ a, pub2 ≅ 7 ^ b
    [pub1, pub2] <- pure . rights $ readEntire T.decimal <$> T.words input
    -- 7 ^ x ≅ pub1 / pub2, 7 ^ y ≅ pub1 * pub2
    [x, y] <- elemIndexAll [pub1 /% pub2, pub1 *% pub2] $ iterate (7 *%) 1
    let -- 7 ^ n ≅ pub1 ^ 2 = 7 ^ 2a, 7 ^ m ≅ pub2 ^ 2 = 7 ^ 2b
        (n, m) = (y + x, y - x)
        -- k ≅ pub1 ^ 2b = 7 ^ 2ab, k' ≅ pub2 ^ 2a = 7 ^ 2ab
        (k, k') = (pub1 ^% m, pub2 ^% n)
    guard $ k == k'
    -- k ≅ ± 7 ^ ab = ± pub1 ^ b = ± pub2 ^ a
    k'' <- elemIndex k [i *% i | i <- [0 :: Integer ..]]
    let k''' = (-k'') `mod` 20201227
    traceShowM (k'', k''')
    return k'''
