{-|
Module:         Day24
Description:    <https://adventofcode.com/2020/day/24 Day 24: Lobby Layout>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, TypeFamilies #-}
module Day24 (day24a, day24b) where

import Data.Bits ((.&.))
import Data.List (foldl')
import qualified Data.IntMap.Strict as IntMap (filterWithKey, fromListWith, keysSet)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (delete, empty, insert, member, size, toList)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, choice, eof, many, parse, some)
import Text.Megaparsec.Char (char, space, string)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m IntSet
parser = foldl' insertOrRemove IntSet.empty <$> many (line <* space) where
    line = foldl' (+&) 0 <$> some direction
    direction = choice
      [ 0x00010000 <$ char 'e',    0x00000001 <$ string "ne"
      , 0x7fff0000 <$ char 'w',    0x00007fff <$ string "sw"
      , 0x00017fff <$ string "se", 0x7fff0001 <$ string "nw"
      ]
    insertOrRemove s k
      | IntSet.member k s = IntSet.delete k s
      | otherwise = IntSet.insert k s

(+&) :: Int -> Int -> Int
a +& b = (a + b) .&. 0x7fff7fff

step :: IntSet -> IntSet
step s = IntMap.keysSet $ IntMap.filterWithKey ok $ IntMap.fromListWith (+)
  [ (p +& d, 1 :: Int)
  | p <- IntSet.toList s
  , d <- [0x1, 0x7fff, 0x10000, 0x17fff, 0x7fff0000, 0x7fff0001]
  ] where
    ok pos 1 = pos `IntSet.member` s
    ok _ 2 = True
    ok _ _ = False

day24a :: Text -> Either (ParseErrorBundle Text Void) Int
day24a = fmap IntSet.size . parse (parser <* eof) ""

day24b :: Text -> Either (ParseErrorBundle Text Void) Int
day24b input = do
    s0 <- parse (parser <* eof) "" input
    pure . IntSet.size $ iterate step s0 !! 100
