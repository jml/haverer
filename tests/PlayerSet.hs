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

module PlayerSet (suite) where

import BasicPrelude

import qualified Data.Set as Set

import Haverer.PlayerSet (toPlayerSet, toPlayers, PlayerSetError(..))
import Haverer.Testing (shuffled)

import Test.Tasty
import Test.Tasty.QuickCheck


instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
  arbitrary = sized $ \n -> do k <- choose (0, n)
                               sizedSet k


sizedSet :: (Arbitrary a, Ord a) => Int -> Gen (Set.Set a)
sizedSet 0 = return Set.empty
sizedSet n = do
  smaller <- sizedSet (n - 1)
  x <- arbitrary `suchThat` \y -> y `Set.notMember` smaller
  return $ Set.insert x smaller


uniqueVector :: (Arbitrary a, Ord a) => Int -> Gen [a]
uniqueVector n = shuffled =<< Set.toList <$> sizedSet n


vectorWithDuplicates' :: (Arbitrary a, Ord a) => Int -> Int -> Gen [a]
vectorWithDuplicates' n m = do
  base <- vector (n - m)
  dupes <- sequence [ elements base | _ <- [1..m] ]
  shuffled $ base ++ dupes


vectorWithDuplicates :: (Arbitrary a, Ord a) => Int -> Gen [a]
vectorWithDuplicates n = do
  numDupes <- choose (1, n - 1)
  vectorWithDuplicates' n numDupes


suite :: TestTree
suite = testGroup "Haverer.PlayerSet" [
  testGroup "QuickCheck tests"
  [ testProperty "one player maketh not a set" $
    forAll (vector 1) $ \x -> toPlayerSet (x :: [Int]) == Left (InvalidNumPlayers 1)
  , testProperty "more than four players can't play" $
    forAll (choose (5, 100) >>= uniqueVector) $
    \x -> toPlayerSet (x :: [Int]) == Left (InvalidNumPlayers (length x))
  , testProperty "can't play with duplicates" $
    forAll (choose (2, 4) >>= vectorWithDuplicates) $
    \xs -> toPlayerSet (xs :: [Int]) == Left (DuplicatePlayers xs)
  , testProperty "all unique groups with 2-4 players are good" $
    forAll (choose (2, 4) >>= uniqueVector) $
    \xs -> (toPlayers <$> toPlayerSet (xs :: [Int])) == Right xs
  ]
  ]
