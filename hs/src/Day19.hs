{-|
Module:         Day19
Description:    <https://adventofcode.com/2020/day/19 Day 19: Monster Messages>
-}
{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
module Day19 (ConstructError, day19a, day19b) where

import Control.DeepSeq (NFData(rnf))
import Control.Monad (foldM, void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap ((!?), delete, findWithDefault, fromList, lookup)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T (reverse)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, ShowErrorComponent(showErrorComponent), Token, Tokens, (<|>), between, choice, chunk, customFailure, eof, many, parse, parseMaybe, sepBy, sepEndBy, skipCount, skipMany, skipSome, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data ConstructError = ConstructError deriving (Eq, Ord, Show)
instance NFData ConstructError where rnf !_ = ()
instance ShowErrorComponent ConstructError where
    showErrorComponent _ = "Cannot construct rule"

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (IntMap [[Either Int (Tokens s)]], [Tokens s])
parser = do
    rules <- many $ parseRule <* newline
    skipSome newline
    messages <- takeWhile1P Nothing (/= '\n') `sepEndBy` newline
    pure (IntMap.fromList rules, messages)
  where
    parseRule = (,) <$> decimal <* char ':' <* hspace <*> parseExp
    parseExp = try parseExp' `sepBy` (char '|' *> hspace)
    parseExp' = many $ parseExp'' <* hspace
    parseExp'' = Left <$> try decimal <|> Right <$>
        between (try $ char '"') (char '"') (takeWhileP Nothing (/= '"'))
    hspace = skipMany $ char ' '

fixRules :: (MonadParsec Void s m) => IntMap [[Either Int (Tokens s)]] -> Maybe (IntMap (m ()))
fixRules rules = traverse id rules' where
    rules' = expand <$> rules
    expand = fmap choice . mapM (fmap try . expand')
    expand' = flip foldM (pure ()) $ \a b -> (a *>) <$> expand'' b
    expand'' = either (IntMap.findWithDefault Nothing `flip` rules') $
        Just . void . chunk

day19a :: Text -> Either (ParseErrorBundle Text ConstructError) Int
day19a = flip parse "" $ do
    (rules, messages) <- parser
    rule <- maybe (customFailure ConstructError) pure $
        fixRules rules >>= IntMap.lookup 0
    pure . length $ mapMaybe (parseMaybe $ rule *> eof) messages

day19b :: Text -> Either (ParseErrorBundle Text ConstructError) Int
day19b = flip parse "" $ do
    (rules, messages) <- parser
    rule <- maybe (customFailure ConstructError) pure $ do
        [[Left 8, Left 11]] <- rules IntMap.!? 0
        [[Left 42]] <- rules IntMap.!? 8
        [[Left 42, Left 31]] <- rules IntMap.!? 11
        rules' <- fixRules $ (map $ reverse . map (fmap T.reverse)) <$>
                foldr IntMap.delete rules [0, 8, 11]
        rule31 <- rules' IntMap.!? 31
        rule42 <- rules' IntMap.!? 42
        let rule n = try (skipCount n rule42 <* many rule42) <|>
                rule31 *> rule (n + 1)
        pure $ rule31 *> rule 2
    pure . length . mapMaybe (parseMaybe $ rule *> eof) $ T.reverse <$> messages
