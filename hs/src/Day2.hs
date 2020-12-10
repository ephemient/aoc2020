{-|
Module:         Day2
Description:    <https://adventofcode.com/2020/day/2 Day 2: Password Philosophy>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Day2 (day2a, day2b) where

import Data.Text (Text)
import qualified Data.Text as T (count, index, singleton)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, anySingleBut, chunk, eof, parse, sepEndBy, single, takeWhileP)
import Text.Megaparsec.Char.Lexer (decimal)

data Rule = Rule { lo :: Int, hi :: Int, char :: Char, text :: Text }

parser :: (MonadParsec e Text m) => m [Rule]
parser = flip sepEndBy (single '\n') $ Rule <$>
    (decimal <* single '-') <*> (decimal <* single ' ') <*>
    (anySingleBut '\n' <* chunk ": ") <*> takeWhileP Nothing (/= '\n')

day2a :: Text -> Either (ParseErrorBundle Text Void) Int
day2a input = length . filter isValid <$> parse (parser <* eof) "" input where
    isValid Rule {..} =
        let n = T.count (T.singleton char) text in lo <= n && n <= hi

day2b :: Text -> Either (ParseErrorBundle Text Void) Int
day2b input = length . filter isValid <$> parse (parser <* eof) "" input where
    isValid Rule {..} =
            (T.index text (lo - 1) == char) /= (T.index text (hi - 1) == char)
