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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haverer.Action (
  BadPlay,
  Play(..),
  Action,
  bustingHand,
  getTarget,
  getValidPlays,
  playToAction,
  viewAction
  ) where

import BasicPrelude
import Control.Monad.Except

import Haverer.Deck (Card(..))


-- | A thing that can be done with a card.
data Play target = NoEffect | Attack target | Guess target Card deriving (Eq, Show)

-- | A validated card + play combination.
-- Only guarantees that such a thing makes sense according to the rules,
-- rather than the current state of the round.
data Action target = Action target Card (Play target) deriving (Eq, Show)

data BadPlay target = BadActionForCard (Play target) Card  -- ^ If that play and card are forbidden by the rules
                    | BadGuess -- ^ If they try to guess a soldier
                    | SelfTarget -- ^ If they try to target themselves when forbidden
                    deriving Show


viewAction :: Action target -> (target, Card, Play target)
viewAction (Action pid card play) = (pid, card, play)


getTarget :: Play target -> Maybe target
getTarget NoEffect = Nothing
getTarget (Attack target) = Just target
getTarget (Guess target _) = Just target


-- | Given a player, a card, and a choice of play, decide whether it's a valid
-- action.
playToAction :: (Eq target, MonadError (BadPlay target) m) => target -> Card -> Play target -> m (Action target)
playToAction pid card play = Action pid card <$> _validatePlay pid card play


_validatePlay :: (Eq target, MonadError (BadPlay target) m) => target -> Card -> Play target -> m (Play target)
_validatePlay _ Soldier (Guess _ Soldier) = throwError BadGuess
_validatePlay player Soldier play@(Guess target _)
  | player == target = throwError SelfTarget
  | otherwise = return play
_validatePlay player Clown play@(Attack target)
  | player == target = throwError SelfTarget
  | otherwise = return play
_validatePlay player Knight play@(Attack target)
  | player == target = throwError SelfTarget
  | otherwise = return play
_validatePlay _ Priestess NoEffect = return NoEffect
_validatePlay _ Wizard play@(Attack _) = return play
_validatePlay player General play@(Attack target)
  | player == target = throwError SelfTarget
  | otherwise = return play
_validatePlay _ Minister NoEffect = return NoEffect
_validatePlay _ Prince NoEffect = return NoEffect
_validatePlay _ card play = throwError (BadActionForCard play card)


-- | Return all valid plays for a particular card.
getValidPlaysForCard :: target -- ^ The current player
                     -> [target] -- ^ All other active players in the round
                     -> Card -- ^ The card they wish to play
                     -> [Play target] -- ^ All valid plays for that card
getValidPlaysForCard self others card =
  case card of
   Soldier   -> [Guess tgt c | tgt <- others, c <- [Clown ..]]
   Clown     -> fmap Attack others
   Knight    -> fmap Attack others
   Priestess -> [NoEffect]
   Wizard    -> fmap Attack (self:others)
   General   -> fmap Attack others
   Minister  -> [NoEffect]
   Prince    -> [NoEffect]


-- | Return all valid plays for a hand.
--
-- If the hand is one that would bust out (see 'bustingHand') then returns an
-- empty list.
getValidPlays :: player -- ^ The current player
              -> [player] -- ^ All other active players in the round
              -> Card -- ^ The card they were dealt
              -> Card -- ^ The card that was in their hand
              -> [(Card, Play player)] -- ^ All valid plays
getValidPlays self others dealt hand = do
  guard $ not $ bustingHand dealt hand
  [(dealt, play) | play <- playsForCard dealt] ++
    [(hand, play) | play <-playsForCard hand]
  where playsForCard = getValidPlaysForCard self others


-- | If you're holding the Minister, there's a potential to "bust out" -- to
-- have to immediately leave the round because you're holding another high
-- card.
bustingHand :: Card -> Card -> Bool
bustingHand Minister card = card >= Wizard
bustingHand card Minister = bustingHand Minister card
bustingHand _ _ = False
