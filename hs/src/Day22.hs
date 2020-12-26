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
import qualified Data.IntMap as IntMap (empty, insert, lookup)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq (empty, fromList, length, mapWithIndex, take)
import qualified Data.IntSet as IntSet (empty, insert, member)
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
    snd $ evalState (evalStateT (game as bs) IntSet.empty) IntMap.empty where
    game Empty Empty = pure (EQ, Seq.empty)
    game Empty bs' = pure (LT, bs')
    game as' Empty = pure (GT, as')
    game as'@(a :<| as'') bs'@(b :<| bs'') = do
        seen <- gets $ IntSet.member hash'
        if seen then pure (GT, as') else do
            modify' $ IntSet.insert hash'
            lift (compare' a as'' b bs'') >>= \case
                LT -> game as'' (bs'' |> b |> a)
                GT -> game (as'' |> a |> b) bs''
      where hash' = hash as' bs'
    compare' a as' b bs'
      | a > Seq.length as' || b > Seq.length bs' = pure $ compare a b
      | a > 0, b > 0, maxA <- maximum as'', maxA > maximum bs'', maxA >= a + b
      = pure GT
      | otherwise = gets (IntMap.lookup hash') >>= \case
        Just cached -> pure cached
        Nothing -> do
            (cmp, _) <- evalStateT (game as'' bs'') IntSet.empty
            cmp <$ modify' (IntMap.insert hash' cmp)
      where
        as'' = Seq.take a as'
        bs'' = Seq.take b bs'
        hash' = hash as'' bs''
    hash as' bs' = foldl' ((+) . (31 *)) (31 * foldl' ((+) . (31 *)) 0 as') bs'

day22b :: Text -> Either (ParseErrorBundle Text Void) Int
day22b input = do
    (as, bs) <- parse (parser <* eof) "" input
    let cs = (playRecursive `on` Seq.fromList) as bs
        n = Seq.length cs
    pure . foldl' (+) 0 $ Seq.mapWithIndex ((*) . (n -)) cs
