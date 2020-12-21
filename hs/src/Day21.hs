{-|
Module:         Day21
Description:    <https://adventofcode.com/2020/day/21 Day 21: Allergen Assessment>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Day21 (day21a, day21b) where

import Data.Char (isAlphaNum)
import Data.List (foldl1', sort)
import qualified Data.Map as Map (elems, fromListWith, null, partition, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set ((\\), findMin, fromList, intersection, notMember, size, unions)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T (intercalate)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, between, eof, optional, parse, sepBy1, sepEndBy, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, newline, string)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m [([Tokens s], [Tokens s])]
parser = parseLine `sepEndBy` newline where
    parseLine = do
        ingredients <- takeWhile1P Nothing isAlphaNum `sepEndBy1` char ' '
        allergens <- optional $ between (string "(contains ") (char ')') $
            takeWhile1P Nothing isAlphaNum `sepBy1` string ", "
        pure (ingredients, fromMaybe [] allergens)

day21a :: Text -> Either (ParseErrorBundle Text Void) Int
day21a input = do
    inputs <- parse (parser <* eof) "" input
    let mapping = foldl1' Set.intersection <$> Map.fromListWith (<>)
            [(k, [Set.fromList vs]) | (vs, ks) <- inputs, k <- ks]
        exclude = Set.unions $ Map.elems mapping
    pure . length . filter (`Set.notMember` exclude) $ concatMap fst inputs 

day21b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Text)
day21b input = do
    inputs <- parse (parser <* eof) "" input
    let mapping = foldl1' Set.intersection <$> Map.fromListWith (<>)
            [(k, [Set.fromList vs]) | (vs, ks) <- inputs, k <- ks]
        loop kvs
          | Map.null kvs = pure []
          | Map.null done = mempty
          | otherwise = (Map.toList (Set.findMin <$> done) ++) <$>
                loop ((Set.\\ Set.unions (Map.elems done)) <$> todo)
          where (done, todo) = Map.partition ((== 1) . Set.size) kvs
    pure . fmap (T.intercalate "," . map snd . sort) $ loop mapping
