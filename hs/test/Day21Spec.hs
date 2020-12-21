{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day21 (day21a, day21b)
import Test.Hspec (Spec, describe, it, shouldBe, it)

example :: Text
example = T.unlines
  [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  , "trh fvjkl sbzzf mxmxvkd (contains dairy)"
  , "sqjhc fvjkl (contains soy)"
  , "sqjhc mxmxvkd sbzzf (contains fish)"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day21a example `shouldBe` Right 5
    describe "part 2" $ do
        it "examples" $ do
            day21b example `shouldBe` Right (Just "mxmxvkd,sqjhc,fvjkl")
