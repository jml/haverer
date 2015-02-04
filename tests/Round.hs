{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Control.Applicative ((<*>), (<$>))
import Data.List (delete)
import Data.Maybe (fromJust)

import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Deck (baseCards, Complete, Deck, makeDeck)
import Haverer.Player (makePlayerSet, PlayerSet)
import Haverer.Round

-- XXX: Move ring tests to separate module

instance Arbitrary (Deck Complete) where
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


-- XXX: Can probably do better than this by generating a shuffled list of
-- integers and then using System.Random.Shuffle.shuffle.
shuffled :: Eq a => [a] -> Gen [a]
shuffled [] = return []
shuffled xs = do x <- oneof $ map return xs
                 ys <- shuffled $ delete x xs
                 return (x:ys)


instance Arbitrary PlayerSet where
  arbitrary = fmap (fromJust . makePlayerSet) (elements [2, 3, 4])

instance Arbitrary Round where
  arbitrary = newRound <$> arbitrary <*> arbitrary



suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
    [ testProperty "allCardsPresent" allCardsPresent ]
  ]
