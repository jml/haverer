{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty
import Test.Tasty.QuickCheck


import Haverer.Ring

instance (Arbitrary a) => Arbitrary (Ring a) where
  arbitrary = fmap newRing (listOf1 arbitrary)


advanceLoops :: (Eq a) => Ring a -> Bool
advanceLoops ring = ring == (iterate advance ring !! ringSize ring)

recoverInput :: (Eq a) => NonEmptyList a -> Bool
recoverInput (NonEmpty xs) = xs == take (length xs) (map currentItem (iterate advance (newRing xs)))

suite :: TestTree
suite = testGroup "Test Suite" [

    testGroup "QuickCheck tests"
    [ testProperty "advanceLoops" $ \x -> advanceLoops (x :: Ring Int)
    , testProperty "recoverInput" $ \x -> recoverInput (x :: NonEmptyList Int)
    ]
  ]

main :: IO ()
main = defaultMain suite
