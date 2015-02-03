{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.List
import Haverer.Ring

-- XXX: Move ring tests to separate module

instance (Arbitrary a) => Arbitrary (Ring a) where
  arbitrary = fmap (fromJust . newRing) (listOf1 arbitrary)


-- If we advance as many times as there are items, we end up at the start.
advanceLoops :: (Eq a) => Ring a -> Bool
advanceLoops ring = ring == (iterate advance ring !! ringSize ring)

-- We can retrieve all of the inputs to a ring by advancing through the ring.
recoverInput :: (Eq a) => NonEmptyList a -> Bool
recoverInput (NonEmpty xs) = xs == (retrieveItems . fromJust . newRing) xs

-- Helper: get all items in ring
retrieveItems :: Ring a -> [a]
retrieveItems r = take (ringSize r) (map currentItem (iterate advance r))

-- Dropping an item from a ring and retrieving all the items is equivalent to
-- deleting an element from a list, unless it's the last item in the ring.
dropping :: (Eq a) => Ring a -> a -> Property
dropping r x =
  not (isOnlyItem r x) ==>
  Just (delete x (retrieveItems r)) == retrieveItems `fmap` dropItem r x

-- Dropping the last item in a ring returns Nothing.
dropLast :: (Eq a) => a -> Bool
dropLast x = dropItem (fromJust (newRing [x])) x == Nothing

-- Helper: Is x the only item in ring r?
isOnlyItem :: Eq a => Ring a -> a -> Bool
isOnlyItem r x = ringSize r == 1 && currentItem r == x

-- If we drop every item from a ring, we get Nothing
dropAllEmpties :: Eq a => Ring a -> Bool
dropAllEmpties ring =
  Nothing == foldM dropItem ring (retrieveItems ring)

-- Dropping the current item will always advance to the next item.
dropCurrentItem :: Eq a => Ring a -> Property
dropCurrentItem ring =
  ringSize ring > 1 ==>
  Just (currentItem (advance ring)) == fmap currentItem (dropItem ring (currentItem ring))

-- Dropping a non-item leaves the ring unchanged
dropNonItem :: Eq a => a -> Ring a -> Property
dropNonItem item ring =
  not (item `elem` retrieveItems ring) ==> dropItem ring item == Just ring

suite :: TestTree
suite = testGroup "Test Suite" [

    testGroup "QuickCheck tests"
    [ testProperty "advanceLoops" $ \x -> advanceLoops (x :: Ring Int)
    , testProperty "recoverInput" $ \x -> recoverInput (x :: NonEmptyList Int)
    , testProperty "dropping" $ \x -> dropping (x :: Ring Int)
    , testProperty "dropLast" $ \x -> dropLast (x :: Ring Int)
    , testProperty "dropAllEmpties" $ \x -> dropAllEmpties (x :: Ring Int)
    , testProperty "dropNonItem" $ \x -> dropNonItem (x :: Int)
    , testProperty "dropCurrentItem" $ \x -> dropCurrentItem (x :: Ring Int)
    ]
  ]

main :: IO ()
main = defaultMain suite
