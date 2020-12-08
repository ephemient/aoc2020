module Day8Spec (spec) where

import Day8 (day8a, day8b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day8a example `shouldBe` Right (Just 5)
    describe "part 2" $ do
        it "examples" $ do
            day8b example `shouldBe` Right (Just 8)
