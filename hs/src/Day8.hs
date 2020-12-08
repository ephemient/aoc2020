{-|
Module:         Day8
Description:    <https://adventofcode.com/2020/day/8 Day 8: Handheld Halting>
-}
{-# LANGUAGE TypeApplications #-}
module Day8 (day8a, day8b) where

import Data.Bool (bool)
import Data.IntSet (empty, insert, member)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V (length)
import Data.Void (Void)
import Machine (Op(Acc, Jmp, Nop), parser, step)
import Text.Megaparsec (ParseErrorBundle, eof, parse)

day8a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day8a input = do
    instructions <- parse (parser @Vector <* eof) "" input
    let loop acc ip seen
          | ip `member` seen = Just acc
          | otherwise = do
                (acc', ip') <- step instructions (acc, ip)
                loop acc' ip' $ insert ip seen
    pure $ loop 0 0 empty

day8b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day8b input = do
    instructions <- parse (parser <* eof) "" input
    let loop acc ip seen mut stack
          | ip < 0 || V.length instructions <= ip
          = if mut then Just acc else maybeLoop stack
          | Acc x <- instructions ! ip
          = (loop $! acc + x) (ip + 1) seen mut stack
          | ip `member` seen = maybeLoop stack
          | Jmp x <- instructions ! ip = loop acc (ip + x) seen' mut
                (bool ((acc, ip + 1, seen', True):) id mut stack)
          | Nop x <- instructions ! ip = loop acc (ip + 1) seen' mut
                (bool ((acc, ip + x, seen', True):) id mut stack)
          where seen' = insert ip seen
        maybeLoop ((acc, ip, seen, mut):stack) = loop acc ip seen mut stack
        maybeLoop _ = Nothing
    pure $ loop 0 0 empty False []
