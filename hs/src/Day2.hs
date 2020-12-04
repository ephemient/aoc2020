{-|
Module:         Day2
Description:    <https://adventofcode.com/2020/day/2 Day 2: Password Philosophy>
-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Day2 (day2a, day2b) where

import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, anySingleBut, chunk, eof, many, parse, sepEndBy, single)
import Text.Megaparsec.Char.Lexer (decimal)

data Rule = Rule { lo :: Int, hi :: Int, char :: Char, string :: String }

parser :: (MonadParsec e String m) => m [Rule]
parser = flip sepEndBy (single '\n') $ Rule <$>
    (decimal <* single '-') <*> (decimal <* single ' ') <*>
    (anySingleBut '\n' <* chunk ": ") <*> many (anySingleBut '\n')

day2a :: String -> Either (ParseErrorBundle String Void) Int
day2a input = length . filter isValid <$> parse (parser <* eof) "" input
  where isValid Rule {..} = let n = length (filter (== char) string) in lo <= n && n <= hi

day2b :: String -> Either (ParseErrorBundle String Void) Int
day2b input = length . filter isValid <$> parse (parser <* eof) "" input
  where isValid Rule {..} = (string !! (lo - 1) == char) /= (string !! (hi - 1) == char)
