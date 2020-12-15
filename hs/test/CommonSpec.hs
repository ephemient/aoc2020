{-# LANGUAGE TypeApplications #-}
module CommonSpec (spec) where

import Common (crt, egcd)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Large(Large), (===), arbitrarySizedIntegral, conjoin, counterexample, cover, property, suchThat)

spec :: Spec
spec = do
    describe "crt" $ do
        prop "is correct" $ do
            q1 <- arbitrarySizedIntegral `suchThat` (/= 0)
            q2 <- arbitrarySizedIntegral `suchThat` (/= 0)
            let q = gcd q1 q2
            r1 <- arbitrarySizedIntegral `suchThat` ((== 0) . (`mod` q))
            r2 <- arbitrarySizedIntegral `suchThat` ((== 0) . (`mod` q))
            let (r3, q3) = crt @Int (r1, q1) (r2, q2)
            pure . cover 90 (abs q1 /= 1 && abs q1 /= 1) "non-trivial" .
                counterexample ("(r1,q1) = " ++ show (r1, q1)) .
                counterexample ("(r2,q2) = " ++ show (r2, q2)) .
                counterexample ("(r3,q3) = " ++ show (r3, q3)) $ conjoin
              [ counterexample "r1 == r3 (mod q1)" $ r1 `mod` q1 === r3 `mod` q1
              , counterexample "q3 == 0 (mod q1)" $ q3 `mod` q1 === 0
              , counterexample "r2 == r3 (mod q2)" $ r2 `mod` q2 === r3 `mod` q2
              , counterexample "q3 == 0 (mod q2)" $ q3 `mod` q2 === 0
              , counterexample "|r3| < |q3|" $ property $ abs r3 < abs q3
              , counterexample "|gcd(r1 - r2, q1, q2)| == |q1 * q2 / q3|" $
                    abs (gcd (r1 - r2) $ gcd q1 q2) === abs (q1 * q2 `div` q3)
              ]
    describe "egcd" $ do
        prop "is correct" $ \(Large a) (Large b) ->
            cover 90 (abs a > 1 && abs b > 1) "non-trivial" $
            let (s, t, g) = egcd @Int a b
             in a * s + b * t === g
