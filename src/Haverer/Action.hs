module Haverer.Action (
  BadPlay,
  Play(..),
  Action,
  playToAction,
  viewAction
  ) where

import Haverer.Deck (Card(..))
import Haverer.Player (PlayerId)


-- | A thing that can be done with a card.
data Play = NoEffect | Attack PlayerId | Guess PlayerId Card deriving Show

-- | A validated card + play combination.
-- Only guarantees that such a thing makes sense according to the rules,
-- rather than the current state of the round.
data Action = Action PlayerId Card Play deriving Show

data BadPlay = BadActionForCard Play Card  -- ^ If that play and card are forbidden by the rules
             | BadGuess -- ^ If they try to guess a soldier
             | SelfTarget -- ^ If they try to target themselves when forbidden
             deriving Show


viewAction :: Action -> (PlayerId, Card, Play)
viewAction (Action pid card play) = (pid, card, play)


playToAction :: PlayerId -> Card -> Play -> Either BadPlay Action
playToAction pid card play =
  Action pid card `fmap` _validatePlay pid card play


_validatePlay :: PlayerId -> Card -> Play -> Either BadPlay Play
_validatePlay _ Soldier (Guess _ Soldier) = Left BadGuess
_validatePlay player Soldier play@(Guess target _)
  | player == target = Left SelfTarget
  | otherwise = Right play
_validatePlay player Clown play@(Attack target)
  | player == target = Left SelfTarget
  | otherwise = Right play
_validatePlay player Knight play@(Attack target)
  | player == target = Left SelfTarget
  | otherwise = Right play
_validatePlay _ Priestess NoEffect = Right NoEffect
_validatePlay _ Wizard play@(Attack _) = Right play
_validatePlay player General play@(Attack target)
  | player == target = Left SelfTarget
  | otherwise = Right play
_validatePlay _ Minister NoEffect = Right NoEffect
_validatePlay _ Prince NoEffect = Right NoEffect
_validatePlay _ card play = Left (BadActionForCard play card)
