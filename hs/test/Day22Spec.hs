{-# LANGUAGE OverloadedStrings #-}
module Day22Spec (spec) where

import Data.Function (on)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq (empty, fromList, length, take)
import qualified Data.Set as Set (empty, insert, member)
import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day22 (day22a, day22b, playRecursive)
import Test.Hspec (Spec, describe, it, shouldBe, it)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck ((===), counterexample, getSize, shuffle, suchThat, suchThatMap)

example :: Text
example = T.unlines
  [ "Player 1:", "9", "2", "6", "3", "1", ""
  , "Player 2:", "5", "8", "4", "7", "10"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day22a example `shouldBe` Right 306
    describe "part 2" $ do
        it "examples" $ do
            day22b example `shouldBe` Right 291
    describe "QuickCheck" $ do
        modifyMaxSize (`div` 5) $ prop "prop" $ do
            n <- getSize `suchThat` (> 0)
            ((as, bs), expected) <-
                (splitAt n <$> shuffle [1..2 * n]) `suchThatMap`
                    \input@(as, bs) -> (,) input <$> naive as bs
            pure $ counterexample (show (as, bs)) $
                 expected === (playRecursive `on` Seq.fromList) as bs

naive :: [Int] -> [Int] -> Maybe (Seq Int)
naive as bs = snd <$> (naive' Set.empty `on` Seq.fromList) as bs where
    naive' _ Empty Empty = Just (EQ, Seq.empty)
    naive' _ Empty bs' = Just (LT, bs')
    naive' _ as' Empty = Just (GT, as')
    naive' seen as' bs' | Set.member (as', bs') seen = Just (GT, as')
    naive' seen as'@(a :<| as'') bs'@(b :<| bs'')
      | Just LT <- cmp = naive' seen' as'' (bs'' |> b |> a)
      | Just GT <- cmp = naive' seen' (as'' |> a |> b) bs''
      | otherwise = Nothing
      where
        cmp
          | a > Seq.length as'' || b > Seq.length bs'' = pure $ compare a b
          | otherwise = fst <$> naive' seen' (Seq.take a as'') (Seq.take b bs'')
        seen' = Set.insert (as', bs') seen
