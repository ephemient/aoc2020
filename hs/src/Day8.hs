{-|
Module:         Day8
Description:    <https://adventofcode.com/2020/day/8 Day 8: Handheld Halting>
-}
{-# LANGUAGE NondecreasingIndentation, TypeApplications #-}
module Day8 (day8a, day8b) where

import Data.Maybe (listToMaybe, maybeToList)
import Data.Set as S (empty, insert, member)
import Data.Vector (Vector, (//), toList)
import Data.Void (Void)
import Machine (Op(Jmp, Nop), parser, step)
import Text.Megaparsec (ParseErrorBundle, eof, parse)

day8a :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day8a input = do
    insns <- parse (parser @Vector <* eof) "" input
    let loop state seen = do
            state'@(acc, ip) <- step insns state
            if ip `member` seen then pure acc else
                loop state' $ insert ip seen
    pure $ loop (0, 0) empty

day8b :: String -> Either (ParseErrorBundle String Void) (Maybe Int)
day8b input = do
    insns <- parse (parser <* eof) "" input
    pure . listToMaybe $ do
    (i, insn) <- zip [0..] $ toList insns
    insn' <- case insn of
        Jmp x -> [Nop x]
        Nop x -> [Jmp x]
        _ -> []
    let insns' = insns // [(i, insn')]
        loop state@(acc, _) seen = case step insns' state of
            Nothing -> Just acc
            Just state'@(_, ip) -> if ip `member` seen then Nothing else
                loop state' $ insert ip seen
    maybeToList $ loop (0, 0) empty
