{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Control.Applicative ((<*>), (<$>))
import Data.Maybe (fromJust)

import System.Random.Shuffle (shuffle)
import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Deck (baseCards, Complete, Deck, makeDeck)
import Haverer.Player (makePlayerSet, PlayerSet)
import Haverer.Round



instance Arbitrary (Deck Complete) where
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


shuffled ::[a] -> Gen [a]
shuffled xs = do
  rs <- randomOrdering (length xs - 1)
  return $ shuffle xs rs
  where
    -- a sequence (r1,...r[n-1]) of numbers such that r[i] is an independent
    -- sample from a uniform random distribution [0..n-i]
    randomOrdering 0 = return []
    randomOrdering n =
      do y <- choose (0, n)
         ys <- randomOrdering (n - 1)
         return (y:ys)



instance Arbitrary PlayerSet where
  arbitrary = fmap (fromJust . makePlayerSet) (elements [2, 3, 4])

instance Arbitrary Round where
  arbitrary = newRound <$> arbitrary <*> arbitrary



suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
    [ testProperty "allCardsPresent" allCardsPresent ]
  ]
