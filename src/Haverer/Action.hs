module Haverer.Action where

import Haverer.Deck
import Haverer.Player


data Play = NoEffect | Attack PlayerId | Guess PlayerId Card


data Action =
  NoChange |
  Protect PlayerId |
  SwapHands PlayerId PlayerId |
  EliminatePlayer PlayerId |
  ForceDiscard PlayerId |
  ForceReveal PlayerId PlayerId |
  EliminateWeaker PlayerId PlayerId |
  EliminateOnGuess PlayerId Card


data BadPlay = BadActionForCard Play Card | BadGuess | SelfTarget


playToAction :: PlayerId -> Card -> Play -> Either BadPlay Action
playToAction _ Soldier (Guess _ Soldier) = Left BadGuess
playToAction _ Soldier (Guess target guess) = Right $ EliminateOnGuess target guess
playToAction player Clown (Attack target) = Right $ ForceReveal player target
playToAction player Knight (Attack target) = Right $ EliminateWeaker player target
playToAction player Priestess NoEffect = Right $ Protect player
playToAction _ Wizard (Attack target) = Right $ ForceDiscard target
playToAction player General (Attack target) = Right $ SwapHands player target
playToAction _ Minister NoEffect = Right $ NoChange
playToAction player Prince NoEffect = Right $ EliminatePlayer player
playToAction _ card play = Left (BadActionForCard play card)


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
