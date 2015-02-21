{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Haverer.Testing where

import Prelude hiding (round)

import Control.Applicative ((<*>), (<$>))
import Data.Maybe (fromJust)

import System.Random.Shuffle (shuffle)
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..))
import Haverer.Deck (baseCards, Card(..), Complete, Deck, makeDeck)
import Haverer.Player (makePlayerSet, PlayerSet)
import Haverer.Round (
  Round
  , Event(..)
  , newRound
  , playTurn
  )
import Haverer.ValidMoves (getValidMoves)



instance Arbitrary (Deck Complete) where
  -- | An arbitrary complete deck is a shuffled set of cards.
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


instance Arbitrary PlayerSet where
  -- | Start the game with a random number of players.
  arbitrary = fmap (fromJust . makePlayerSet) (elements [2, 3, 4])


instance Arbitrary Round where
  -- | A fresh, unplayed round with an arbitrary number of players and a
  -- shuffled deck.
  arbitrary = newRound <$> arbitrary <*> arbitrary


-- | For a Round and a known-good Card and Play, play the cards and return the
-- round and event. If the hand busts out, Card and Play are ignored.
playTurn' :: Round -> Card -> Play -> (Round, Event)
playTurn' round card play =
  case playTurn round of
   Left (round', event) -> (round', event)
   Right handlePlay ->
     case handlePlay card play of
      Left err -> error $ "Should have generated valid play: " ++ show (err, round, card, play)
      Right (round', event) -> (round', event)


playRandomTurn :: Round -> Gen (Round, Event)
playRandomTurn round = do
  move <- randomCardPlay round
  case move of
   Nothing -> return (round, RoundOver)
   Just (card, play) -> return $ playTurn' round card play
  where
    randomCardPlay round' =
      case getValidMoves round' of
       [] -> return Nothing
       xs -> elements (fmap Just xs)


-- | Given a Round, generate a Round that's randomly had a move applied, i.e.
-- a possible next Round. If there are no valid moves, then return the same
-- Round.
randomNextMove :: Round -> Gen Round
randomNextMove round = fst <$> playRandomTurn round


-- | Generate a sequence of N rounds, starting from an initial round.
manyMoves :: Int -> Gen [Round]
manyMoves 0 = return []
manyMoves n = do
  initial <- arbitrary
  rest <- iterateM' (n - 2) randomNextMove initial
  return (initial:rest)


-- | Generate a random number of consecutive rounds, starting from an initial round.
randomRounds :: Gen [Round]
randomRounds = do
  num <- choose (2, 14)
  manyMoves num


-- | Generate a random round that might come up in the course of play.
randomRound :: Gen Round
randomRound = last <$> randomRounds


-- | Generate a random, non-terminating round and a valid play for that round.
roundAndPlay :: Gen (Round, Card, Play)
roundAndPlay = do
  round <- randomRound `suchThat` (not . null . getValidMoves)
  (card, play) <- elements $ getValidMoves round
  return (round, card, play)


-- | Generate an event that might come up in the course of play.
inRoundEvent :: Gen Event
inRoundEvent = do
  (round, card, play) <- roundAndPlay
  return $ snd $ playTurn' round card play


-- | Take a list and generate a shuffled version of it.
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


-- | Kind of like iterate, but for a monadic function, such that the result of
-- calling once is used as the argument for calling next.
iterateM' :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM' n f x
  | n == 0 = return [x]
  | n > 0  = do y <- f x
                ys <- iterateM' (n - 1) f y
                return (y:ys)
  | otherwise = return []