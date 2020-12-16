{-|
Module:         Day16
Description:    <https://adventofcode.com/2020/day/16 Day 16: Ticket Translation>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, NondecreasingIndentation, OverloadedStrings #-}
module Day16 (day16a, day16b, day16b') where

import Data.Char (isAlphaNum)
import qualified Data.IntSet as IntSet (delete, fromDistinctAscList, intersection, size, toList)
import Data.Ix (inRange)
import Data.List (foldl', foldl1')
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, between, eof, parse, sepBy, sepEndBy, some, takeWhile1P)
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Input = Input { rules :: [Rule], yours :: [Int], nearby :: [[Int]] }
data Rule = Rule { name :: Text, ranges :: [(Int, Int)] }

parser :: (MonadParsec e Text m) => m Input
parser = Input <$> rules <*> yours <*> nearby where
    rules = rule `sepEndBy` newline <* space
    name = takeWhile1P Nothing $ \c -> c == ' ' || isAlphaNum c
    rule = Rule <$> (name <* string ": ") <*>
        (((,) <$> decimal <* char '-' <*> decimal) `sepBy` string " or ")
    ticket = decimal `sepBy` char ',' <* newline
    yours = between (string "your ticket:\n") space ticket
    nearby = between (string "nearby tickets:\n") space $ some ticket

day16a :: Text -> Either (ParseErrorBundle Text Void) Int
day16a input = do
    Input {rules, nearby} <- parse (parser <* eof) "" input
    let isValid num = any (`inRange` num) $ rules >>= ranges
    pure . sum . filter (not . isValid) $ concat nearby

day16b :: Text -> Either (ParseErrorBundle Text Void) Int
day16b = fmap (foldl' (*) 1 . map snd . filter (isDeparture . fst)) . day16b'
  where isDeparture name = "departure" `T.isPrefixOf` name

day16b' :: Text -> Either (ParseErrorBundle Text Void) [(Text, Int)]
day16b' input = do
    Input {rules, yours, nearby} <- parse (parser <* eof) "" input
    pure $ do
    let isValid num = any (`inRange` num) $ rules >>= ranges
        possibleFieldIds num = IntSet.fromDistinctAscList
          [ i
          | (i, Rule {ranges}) <- zip [0..] rules
          , any (`inRange` num) ranges
          ]
        loop options
          | (left, (i, xs):right) <- break ((== 1) . IntSet.size . snd) options
          = let [x] = IntSet.toList xs in (name $ rules !! x, yours !! i) :
                loop (fmap (IntSet.delete x) <$> left ++ right)
        loop _ = []
    loop . zip [0..] . foldl1' (zipWith IntSet.intersection) $
        map possibleFieldIds <$> filter (all isValid) nearby
