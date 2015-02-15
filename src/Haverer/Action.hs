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

import Haverer.Deck (Card(..))
import Haverer.Player (PlayerId)


-- | A thing that can be done with a card.
data Play = NoEffect | Attack PlayerId | Guess PlayerId Card deriving (Eq, Show)

-- | A validated card + play combination.
-- Only guarantees that such a thing makes sense according to the rules,
-- rather than the current state of the round.
data Action = Action PlayerId Card Play deriving (Eq, Show)

data BadPlay = BadActionForCard Play Card  -- ^ If that play and card are forbidden by the rules
             | BadGuess -- ^ If they try to guess a soldier
             | SelfTarget -- ^ If they try to target themselves when forbidden
             deriving Show


viewAction :: Action -> (PlayerId, Card, Play)
viewAction (Action pid card play) = (pid, card, play)


getTarget :: Play -> Maybe PlayerId
getTarget NoEffect = Nothing
getTarget (Attack target) = Just target
getTarget (Guess target _) = Just target


-- | Given a player, a card, and a choice of play, decide whether it's a valid
-- action.
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


-- | Return all valid plays.
getValidPlays :: PlayerId -- ^ The current player
                 -> [PlayerId] -- ^ All other active players in the round
                 -> Card -- ^ The card they wish to play
                 -> [Play] -- ^ All valid plays for that card
getValidPlays self others card =
  case card of
   Soldier   -> [Guess tgt c | tgt <- others, c <- [Clown ..]]
   Clown     -> fmap Attack others
   Knight    -> fmap Attack others
   Priestess -> [NoEffect]
   Wizard    -> fmap Attack (self:others)
   General   -> fmap Attack others
   Minister  -> [NoEffect]
   Prince    -> [NoEffect]


-- | If you're holding the Minister, there's a potential to "bust out" -- to
-- have to immediately leave the round because you're holding another high
-- card.
bustingHand :: Card -> Card -> Bool
bustingHand Minister card = card >= Wizard
bustingHand card Minister = bustingHand Minister card
bustingHand _ _ = False
