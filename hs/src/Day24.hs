{-|
Module:         Day24
Description:    <https://adventofcode.com/2020/day/24 Day 24: Lobby Layout>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, TypeFamilies #-}
module Day24 (day24a, day24b) where

import Data.List (foldl1')
import qualified Data.Map as Map (filter, filterWithKey, fromListWith, keysSet)
import Data.Semigroup (Max(Max), Min(Min))
import Data.Set (Set)
import qualified Data.Set as Set (member, size, toList)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, choice, eof, many, parse, some)
import Text.Megaparsec.Char (char, space, string)
import Text.Printf (printf)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Set (Int, Int))
parser = Map.keysSet . Map.filter odd . Map.fromListWith (+) . map (, 1) <$>
    many (foldl1' (.+) <$> some direction <* space) where
    direction = choice
      [ (1, 0) <$ char 'e', (1, -1) <$ string "se", (0, -1) <$ string "sw"
      , (-1, 0) <$ char 'w', (-1, 1) <$ string "nw", (0, 1) <$ string "ne"
      ]
    (a, b) .+ (c, d) = (a + c, b + d)

step :: Set (Int, Int) -> Set (Int, Int)
step s = Map.keysSet $ Map.filterWithKey ok $ Map.fromListWith (+)
  [ ((x + dx, y + dy), 1)
  | (x, y) <- Set.toList s
  , dx <- [-1..1]
  , dy <- [-1..1]
  , abs (dx + dy) < 2
  ] where
    ok _ 2 = True
    ok pos 3 = pos `Set.member` s
    ok _ _ = False

day24a :: Text -> Either (ParseErrorBundle Text Void) Int
day24a = fmap Set.size . parse (parser <* eof) ""

day24b :: Text -> Either (ParseErrorBundle Text Void) Int
day24b input = do
    s0 <- parse (parser <* eof) "" input
    pure $ Set.size $ iterate step s0 !! 100

showSet :: Set (Int, Int) -> String
showSet s
  | Just (Min minX, Min minY, Max maxX, Max maxY) <-
        mconcat $ Just . proj <$> Set.toList s
  = unlines $ printf "%d (%d..%d, %d..%d)" (Set.size s) minX maxX minY maxY :
      [ [ if d /= 0
            then if fx == 0 then '|' else if y == 0 then '-' else ' '
            else if (x, y) `Set.member` s then '#' else '.'
        | fx <- [2 * minX..2 * maxX]
        , let (x, d) = (fx - y) `divMod` 2
        ]
      | y <- [minY..maxY]
      ]
  | otherwise = "(empty)"
  where proj (x', y) = (Min x, Min y, Max x, Max y) where x = x' + y
