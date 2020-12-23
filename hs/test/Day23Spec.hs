module Day23Spec (spec) where

import Day23 (day23a, day23b)
import Test.Hspec (Spec, describe, it, shouldBe, it)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day23a "389125467" `shouldBe` 67384529
    describe "part 2" $ do
        it "examples" $ do
            day23b "389125467" `shouldBe` 149245887792
