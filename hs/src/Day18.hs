{-|
Module:         Day18
Description:    <https://adventofcode.com/2020/day/18 Day 18: Operation Order>
-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Day18 (day18a, day18b) where

import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), between, eof, parse, sepEndBy, sepEndBy1, skipMany)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Expr a = Lit a | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [Expr a]
parser = chain `sepEndBy` newline where
    chain = atom >>= expr
    expr lhs =
        lexeme (char '+') *> (atom >>= expr . Add lhs) <|>
        lexeme (char '*') *> (atom >>= expr . Mul lhs) <|>
        pure lhs
    atom = Lit <$> lexeme decimal <|>
        between (lexeme $ char '(') (lexeme $ char ')') chain

parser' :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [Expr a]
parser' = mulChain `sepEndBy` newline where
    mulChain = foldl1 Mul <$> addChain `sepEndBy1` lexeme (char '*')
    addChain = foldl1 Add <$> atom `sepEndBy1` lexeme (char '+')
    atom = Lit <$> lexeme decimal <|>
        between (lexeme $ char '(') (lexeme $ char ')') mulChain

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme a = a <* skipMany (char ' ')

eval :: (Num a) => Expr a -> a
eval (Lit a) = a
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

day18a :: Text -> Either (ParseErrorBundle Text Void) Int
day18a input = do
    exprs <- parse (parser <* eof) "" input
    pure . sum $ eval <$> exprs

day18b :: Text -> Either (ParseErrorBundle Text Void) Int
day18b input = do
    exprs <- parse (parser' <* eof) "" input
    pure . sum $ eval <$> exprs
