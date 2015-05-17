-- Copyright (c) 2014-2015 Jonathan M. Lange <jml@mumak.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ring where

import BasicPrelude

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Maybe (fromJust)

import Haverer.Internal.Ring


instance (Arbitrary a) => Arbitrary (Ring a) where
  arbitrary = fromJust . makeRing <$> listOf1 arbitrary


-- If we advance as many times as there are items, we end up at the start.
advanceLoops :: (Eq a) => Ring a -> Bool
advanceLoops ring = ring == (iterate advance ring !! ringSize ring)

-- We can retrieve all of the inputs to a ring by advancing through the ring.
recoverInput :: (Eq a) => NonEmptyList a -> Bool
recoverInput (NonEmpty xs) = xs == (retrieveItems . fromJust . makeRing) xs

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
dropLast x = isNothing $ dropItem (fromJust (makeRing [x])) x

-- Helper: Is x the only item in ring r?
isOnlyItem :: Eq a => Ring a -> a -> Bool
isOnlyItem r x = ringSize r == 1 && currentItem r == x

-- If we drop every item from a ring, we get Nothing
dropAllEmpties :: Eq a => Ring a -> Bool
dropAllEmpties ring =
  isNothing $ foldM dropItem ring (retrieveItems ring)

-- Dropping the current item will always advance to the next item.
dropCurrentItem :: Eq a => Ring a -> Property
dropCurrentItem ring =
  ringSize ring > 1 ==>
  Just (currentItem (advance ring)) == fmap currentItem (dropItem ring (currentItem ring))

-- Dropping a non-item leaves the ring unchanged
dropNonItem :: Eq a => a -> Ring a -> Property
dropNonItem item ring =
  (item `notElem` retrieveItems ring) ==> dropItem ring item == Just ring

suite :: TestTree
suite = testGroup "Haverer.Ring" [
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
