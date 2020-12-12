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

day12 :: Bool -> Text -> Either String Int
day12 waypoints input = do
    ((x, y), _) <- foldl' move
        ((0, 0), if waypoints then (10, 1) else (1, 0)) <$>
        mapM parse (T.lines input)
    pure $ abs x + abs y
  where
    move (pos@(x, y), dir@(dx, dy)) (Rel (rx, ry))
      | waypoints = (pos, (dx + rx, dy + ry))
      | otherwise = ((x + rx, y + ry), dir)
    move ((x, y), dir@(dx, dy)) (Fwd n) = ((x + n * dx, y + n * dy), dir)
    move (pos, (dx, dy)) L = (pos, (-dy, dx))
    move (pos, (dx, dy)) R = (pos, (dy, -dx))
    move (pos, (dx, dy)) U = (pos, (-dx, -dy))

day12a :: Text -> Either String Int
day12a = day12 False

day12b :: Text -> Either String Int
day12b = day12 True
