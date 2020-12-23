{-|
Module:         Day22
Description:    <https://adventofcode.com/2020/day/22 Day 22: Crab Combat>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TypeFamilies #-}
module Day22 (day22a, day22b, playRecursive) where

import Control.Monad.State (evalState, evalStateT, gets, modify')
import Control.Monad.Trans (lift)
import Data.Function (on)
import Data.List (foldl')
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq (empty, fromList, length, mapWithIndex, take)
import qualified Data.Set as Set (empty, insert, member)
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
play (a :<| as') (b :<| bs') = case compare a b of
    LT -> play as' (bs' |> b |> a)
    GT -> play (as' |> a |> b) bs'

day22a :: Text -> Either (ParseErrorBundle Text Void) Int
day22a input = do
    (as, bs) <- parse (parser <* eof) "" input
    let cs = (play `on` Seq.fromList) as bs
        n = Seq.length cs
    pure . foldl' (+) 0 $ Seq.mapWithIndex ((*) . (n -)) cs

playRecursive :: Seq Int -> Seq Int -> Seq Int
playRecursive as bs =
    snd $ evalState (evalStateT (game as bs) Set.empty) Map.empty where
    game Empty Empty = pure (EQ, Seq.empty)
    game Empty bs' = pure (LT, bs')
    game as' Empty = pure (GT, as')
    game as'@(a :<| as'') bs'@(b :<| bs'') = do
        seen <- gets $ Set.member (as', bs')
        if seen then pure (GT, as') else do
            modify' $ Set.insert (as', bs')
            lift (compare' a as'' b bs'') >>= \case
                LT -> game as'' (bs'' |> b |> a)
                GT -> game (as'' |> a |> b) bs''
    compare' a as' b bs'
      | a > Seq.length as' || b > Seq.length bs' = pure $ compare a b
      | otherwise = gets (Map.lookup (as'', bs'')) >>= \case
        Just cached -> pure cached
        Nothing -> do
            (cmp, _) <- evalStateT (game as'' bs'') Set.empty
            cmp <$ modify' (Map.insert (as'', bs'') cmp)
      where
        as'' = Seq.take a as'
        bs'' = Seq.take b bs'

day22b :: Text -> Either (ParseErrorBundle Text Void) Int
day22b input = do
    (as, bs) <- parse (parser <* eof) "" input
    let cs = (playRecursive `on` Seq.fromList) as bs
        n = Seq.length cs
    pure . foldl' (+) 0 $ Seq.mapWithIndex ((*) . (n -)) cs
