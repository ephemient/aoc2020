{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Day15 (day15)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day15 1 "0,3,6" `shouldBe` Right 0
            day15 2 "0,3,6" `shouldBe` Right 3
            day15 3 "0,3,6" `shouldBe` Right 6
            day15 4 "0,3,6" `shouldBe` Right 0
            day15 5 "0,3,6" `shouldBe` Right 3
            day15 6 "0,3,6" `shouldBe` Right 3
            day15 7 "0,3,6" `shouldBe` Right 1
            day15 8 "0,3,6" `shouldBe` Right 0
            day15 9 "0,3,6" `shouldBe` Right 4
            day15 10 "0,3,6" `shouldBe` Right 0
            day15 2020 "0,3,6" `shouldBe` Right 436
            day15 2020 "1,3,2" `shouldBe` Right 1
            day15 2020 "2,1,3" `shouldBe` Right 10
            day15 2020 "1,2,3" `shouldBe` Right 27
            day15 2020 "2,3,1" `shouldBe` Right 78
            day15 2020 "3,2,1" `shouldBe` Right 438
            day15 2020 "3,1,2" `shouldBe` Right 1836
    describe "part 2" $ do
        it "examples" $ do
            day15 30000000 "0,3,6" `shouldBe` Right 175594
            day15 30000000 "1,3,2" `shouldBe` Right 2578
            day15 30000000 "2,1,3" `shouldBe` Right 3544142
            day15 30000000 "1,2,3" `shouldBe` Right 261214
            day15 30000000 "2,3,1" `shouldBe` Right 6895259
            day15 30000000 "3,2,1" `shouldBe` Right 18
            day15 30000000 "3,1,2" `shouldBe` Right 362
