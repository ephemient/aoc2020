{-|
Module:         Day18
Description:    <https://adventofcode.com/2020/day/18 Day 18: Operation Order>
-}
{-# LANGUAGE BangPatterns, TypeFamilies #-}
module Day18 (day18a, day18b) where

import Data.List (foldl', foldl1')
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, (<|>), between, eof, parse, sepEndBy, sepEndBy1, skipMany)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme a = a <* skipMany (char ' ')

day18a :: Text -> Either (ParseErrorBundle Text Void) Int
day18a = parse (parser <* eof) "" where
    parser = foldl' (+) 0 <$> chain `sepEndBy` newline
    chain = atom >>= expr
    expr !lhs =
        lexeme (char '+') *> (atom >>= expr . (+) lhs) <|>
        lexeme (char '*') *> (atom >>= expr . (*) lhs) <|>
        pure lhs
    atom = lexeme decimal <|>
        between (lexeme $ char '(') (lexeme $ char ')') chain

day18b :: Text -> Either (ParseErrorBundle Text Void) Int
day18b = parse (parser <* eof) "" where
    parser = foldl' (+) 0 <$> mulChain `sepEndBy` newline
    mulChain = foldl1' (*) <$> addChain `sepEndBy1` lexeme (char '*')
    addChain = foldl1' (+) <$> atom `sepEndBy1` lexeme (char '+')
    atom = lexeme decimal <|>
        between (lexeme $ char '(') (lexeme $ char ')') mulChain
