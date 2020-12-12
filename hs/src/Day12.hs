{-|
Module:         Day12
Description:    <https://adventofcode.com/2020/day/12 Day 12: Rain Risk>
-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module Day12 (day12a, day12b) where

import Common (readEntire)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)

data Instruction a = Rel (a, a) | Fwd a | L | R | U

parse :: (Integral a) => Text -> Either String (Instruction a)
parse "L90" = Right L
parse "R270" = Right L
parse "R90" = Right R
parse "L270" = Right R
parse "L180" = Right U
parse "R180" = Right U
parse input = maybe (Left "empty") Right (T.uncons input) >>= \case
    ('N', n) -> Rel . (0,) <$> readEntire T.decimal n
    ('E', n) -> Rel . (, 0) <$> readEntire T.decimal n
    ('S', n) -> Rel . (0,) . negate <$> readEntire T.decimal n
    ('W', n) -> Rel . (, 0) . negate <$> readEntire T.decimal n
    ('F', n) -> Fwd <$> readEntire T.decimal n
    _ -> Left $ "no parse of " ++ show input

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

day12a :: Text -> Either String Int
day12a input = do
    moves <- mapM parse $ T.lines input
    let ((x, y), _) = foldl' move ((0, 0), (1, 0)) moves
    pure $ abs x + abs y

day12b :: Text -> Either String Int
day12b input = do
    moves <- mapM parse $ T.lines input
    let ((x, y), _) = foldl' move2 ((0, 0), (10, 1)) moves
    pure $ abs x + abs y
