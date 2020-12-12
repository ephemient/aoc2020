{-|
Module:         Day12
Description:    <https://adventofcode.com/2020/day/12 Day 12: Rain Risk>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections #-}
module Day12 (day12a, day12b) where

import Data.Functor (($>))
import Data.List (foldl')
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, (<|>), choice, eof, parse, sepEndBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Instruction a = Rel (a, a) | Fwd a | L | R | U

parser :: (Num a, MonadParsec e Text m) => m [Instruction a]
parser = parseLine `sepEndBy` newline where
    parseLine = choice
      [ char 'N' *> (Rel . (0, ) <$> decimal)
      , char 'E' *> (Rel . (, 0) <$> decimal)
      , char 'S' *> (Rel . (0, ) . negate <$> decimal)
      , char 'W' *> (Rel . (, 0) . negate <$> decimal)
      , (string "L90" <|> string "R270") $> L
      , (string "R90" <|> string "L270") $> R
      , (string "L180" <|> string "R180") $> U
      , char 'F' *> (Fwd <$> decimal)
      ]

move :: (Num a) => ((a, a), (a, a)) -> Instruction a -> ((a, a), (a, a))
move ((x, y), dir) (Rel (dx, dy)) = ((x + dx, y + dy), dir)
move ((x, y), dir@(dx, dy)) (Fwd n) = ((x + n * dx, y + n * dy), dir)
move (pos, (dx, dy)) L = (pos, (-dy, dx))
move (pos, (dx, dy)) R = (pos, (dy, -dx))
move (pos, (dx, dy)) U = (pos, (-dx, -dy))

move2 :: (Num a) => ((a, a), (a, a)) -> Instruction a -> ((a, a), (a, a))
move2 (pos, (wx, wy)) (Rel (dx, dy)) = (pos, (wx + dx, wy + dy))
move2 ((x, y), way@(wx, wy)) (Fwd n) = ((x + n * wx, y + n * wy), way)
move2 (pos, (wx, wy)) L = (pos, (-wy, wx))
move2 (pos, (wx, wy)) R = (pos, (wy, -wx))
move2 (pos, (wx, wy)) U = (pos, (-wx, -wy))

day12a :: Text -> Either (ParseErrorBundle Text Void) Int
day12a input = do
    moves <- parse (parser <* eof) "" input
    let ((x, y), _) = foldl' move ((0, 0), (1, 0)) moves
    pure $ abs x + abs y

day12b :: Text -> Either (ParseErrorBundle Text Void) Int
day12b input = do
    moves <- parse (parser <* eof) "" input
    let ((x, y), _) = foldl' move2 ((0, 0), (10, 1)) moves
    pure $ abs x + abs y
