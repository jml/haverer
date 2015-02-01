module Haverer.Action (
  Action(..),
  Play(..),
  playToAction,
  ) where

import Haverer.Deck
import Haverer.Player


data Play = NoEffect | Attack PlayerId | Guess PlayerId Card deriving Show


-- XXX: Currently expose all of these constructors for pattern matching.
-- However, would ideally prefer only *validated* actions to be allowed to be
-- created. I *think* this is the case where phantom types will help.

data Action =
  NoChange |
  Protect PlayerId |
  SwapHands PlayerId PlayerId |
  EliminatePlayer PlayerId |
  ForceDiscard PlayerId |
  ForceReveal PlayerId PlayerId |
  EliminateWeaker PlayerId PlayerId |
  EliminateOnGuess PlayerId Card
  deriving Show


data BadPlay = BadActionForCard Play Card | BadGuess | SelfTarget deriving Show


playToAction :: PlayerId -> Card -> Play -> Either BadPlay Action
playToAction pid card play = _playToAction pid card play >>= validateAction pid


_playToAction :: PlayerId -> Card -> Play -> Either BadPlay Action
_playToAction _ Soldier (Guess _ Soldier) = Left BadGuess
_playToAction _ Soldier (Guess target guess) = Right $ EliminateOnGuess target guess
_playToAction player Clown (Attack target) = Right $ ForceReveal player target
_playToAction player Knight (Attack target) = Right $ EliminateWeaker player target
_playToAction player Priestess NoEffect = Right $ Protect player
_playToAction _ Wizard (Attack target) = Right $ ForceDiscard target
_playToAction player General (Attack target) = Right $ SwapHands player target
_playToAction _ Minister NoEffect = Right $ NoChange
_playToAction player Prince NoEffect = Right $ EliminatePlayer player
_playToAction _ card play = Left (BadActionForCard play card)


getTarget :: Action -> Maybe PlayerId
getTarget NoChange = Nothing
getTarget (Protect _) = Nothing
getTarget (SwapHands _ x) = Just x
getTarget (EliminatePlayer x) = Just x
getTarget (ForceDiscard x) = Just x
getTarget (ForceReveal _ x) = Just x
getTarget (EliminateWeaker _ x) = Just x
getTarget (EliminateOnGuess x _) = Just x


validateAction :: PlayerId -> Action -> Either BadPlay Action
validateAction player action =
  case getTarget action of
   Nothing -> Right action
   Just target ->
     if player == target then
       case action of
        (ForceDiscard _) -> Right action
        _ -> Left SelfTarget
     else Right action
