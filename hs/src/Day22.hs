{-|
Module:         Day22
Description:    <https://adventofcode.com/2020/day/22 Day 22: Crab Combat>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Day22 (day22a) where

import Data.Function (on)
import Data.List (foldl')
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq (fromList, length, mapWithIndex)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, eof, many, parse)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m ([a], [a])
parser = (,) <$
    lexeme (string "Player 1:") <*> many (lexeme decimal) <*
    lexeme (string "Player 2:") <*> many (lexeme decimal)
  where lexeme a = a <* space

play :: (Ord a) => Seq a -> Seq a -> Seq a
play Empty bs = bs
play as Empty = as
play (a :<| as) (b :<| bs) = case compare a b of
    LT -> play as (bs |> b |> a)
    GT -> play (as |> a |> b) bs

day22a :: Text -> Either (ParseErrorBundle Text Void) Int
day22a input = do
    (as, bs) <- parse (parser <* eof) "" input
    let cs = (play `on` Seq.fromList) as bs
        n = Seq.length cs
    pure . foldl' (+) 0 $ Seq.mapWithIndex ((*) . (n -)) cs
