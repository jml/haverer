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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Haverer.Testing ( PlayerId
                       , inRoundEvent
                       , playTurn'
                       , randomRound
                       , randomRounds
                       , shuffled
                       ) where

import BasicPrelude hiding (round)

import Data.Maybe (fromJust)
import qualified System.Random.Shuffle as Shuffle
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..))
import Haverer.Deck (baseCards, Card(..), FullDeck, makeDeck)
import Haverer.Player (PlayerSet, toPlayerSet)
import Haverer.Round (
  Round
  , Result(..)
  , makeRound
  , playTurn
  )
import Haverer.ValidMoves (getValidMoves)
import Haverer.Internal.Error (assertRight)


type PlayerId = Int


instance Arbitrary FullDeck where
  -- | An arbitrary complete deck is a shuffled set of cards.
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


instance Arbitrary (PlayerSet PlayerId) where
  -- | Start the game with a random number of players.
  arbitrary =
    makePlayerSet <$> elements [2, 3, 4]
    where
      makePlayerSet n =
        assertRight "Couldn't make set: " (toPlayerSet $ take n [1..])


instance Arbitrary (Round PlayerId) where
  -- | A fresh, unplayed round with an arbitrary number of players and a
  -- shuffled deck.
  arbitrary = makeRound <$> arbitrary <*> arbitrary


-- | For a Round and a known-good Card and Play, play the cards and return the
-- round and event. If the hand busts out, Card and Play are ignored.
playTurn' :: (Ord a, Show a) => Round a -> Card -> Play a -> (Result a, Round a)
playTurn' round card play = assertRight "Should have generated valid play: " $
  case playTurn round of
   Left action -> action
   Right handler -> handler card play


playRandomTurn :: (Ord a, Show a) => Round a -> Gen (Maybe (Result a, Round a))
playRandomTurn round = do
  move <- randomCardPlay round
  case move of
   Nothing -> return Nothing
   Just (card, play) -> return $ Just $ playTurn' round card play
  where
    randomCardPlay round' =
      case getValidMoves round' of
       [] -> return Nothing
       xs -> elements (fmap Just xs)


-- | Given a Round, generate a Round that's randomly had a move applied, i.e.
-- a possible next Round. If there are no valid moves, then return the same
-- Round.
randomNextMove :: (Ord a, Show a) => Round a -> Gen (Round a)
randomNextMove round = do
  result <- playRandomTurn round
  case result of
   Nothing -> return round
   Just (_, round') -> return round'


-- | Generate a sequence of N rounds, starting from an initial round.
manyMoves :: Int -> Gen [Round PlayerId]
manyMoves 0 = return []
manyMoves n = do
  initial <- arbitrary
  rest <- iterateM' (n - 2) randomNextMove initial
  return (initial:rest)


-- | Generate a random number of consecutive rounds, starting from an initial round.
randomRounds :: Gen [Round PlayerId]
randomRounds = do
  num <- choose (2, 14)
  manyMoves num


-- | Generate a random round that might come up in the course of play.
randomRound :: Gen (Round PlayerId)
randomRound = last <$> randomRounds


-- | Generate a random, non-terminating round and a valid play for that round.
roundAndPlay :: Gen (Round PlayerId, Card, Play PlayerId)
roundAndPlay = do
  round <- randomRound `suchThat` (not . null . getValidMoves)
  (card, play) <- elements $ getValidMoves round
  return (round, card, play)


-- | Generate an event that might come up in the course of play.
inRoundEvent :: Gen (Result PlayerId)
inRoundEvent = do
  (round, card, play) <- roundAndPlay
  return $ fst $ playTurn' round card play


-- | Take a list and generate a shuffled version of it.
shuffled ::[a] -> Gen [a]
shuffled xs = do
  rs <- randomOrdering (length xs - 1)
  return $ Shuffle.shuffle xs rs
  where
    -- a sequence (r1,...r[n-1]) of numbers such that r[i] is an independent
    -- sample from a uniform random distribution [0..n-i]
    randomOrdering 0 = return []
    randomOrdering n =
      do y <- choose (0, n)
         ys <- randomOrdering (n - 1)
         return (y:ys)


-- | Kind of like iterate, but for a monadic function, such that the result of
-- calling once is used as the argument for calling next.
iterateM' :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM' n f x
  | n == 0 = return [x]
  | n > 0  = do y <- f x
                ys <- iterateM' (n - 1) f y
                return (y:ys)
  | otherwise = return []
