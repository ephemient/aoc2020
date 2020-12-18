{-|
Module:         Day18
Description:    <https://adventofcode.com/2020/day/18 Day 18: Operation Order>
-}
{-# LANGUAGE BangPatterns, TypeFamilies #-}
module Day18 (day18a, day18b) where

import Control.Monad.Combinators.Expr (Operator(InfixL), makeExprParser)
import Data.List (foldl')
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, (<|>), between, eof, parse, sepEndBy, skipMany)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

lexeme, parens :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme a = a <* skipMany (char ' ')
parens = between (lexeme $ char '(') (lexeme $ char ')')

addOp, mulOp :: (MonadParsec e s m, Token s ~ Char, Num a) => Operator m a
addOp = InfixL $ (+) <$ lexeme (char '+')
mulOp = InfixL $ (*) <$ lexeme (char '*')

day18a :: Text -> Either (ParseErrorBundle Text Void) Int
day18a = parse (parser <* eof) "" where
    parser = foldl' (+) 0 <$> expr `sepEndBy` newline
    expr = makeExprParser (lexeme decimal <|> parens expr) [[addOp, mulOp]]

day18b :: Text -> Either (ParseErrorBundle Text Void) Int
day18b = parse (parser <* eof) "" where
    parser = foldl' (+) 0 <$> expr `sepEndBy` newline
    expr = makeExprParser (lexeme decimal <|> parens expr) [[addOp], [mulOp]]
