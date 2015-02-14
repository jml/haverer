module Haverer.ValidMoves (getValidMoves) where

import Data.List (delete)
import Prelude hiding (round)

import Haverer.Action (getValidPlays, Play)
import Haverer.Deck (Card)
import Haverer.Round (
  Round,
  currentTurn,
  getActivePlayers
  )


getValidMoves :: Round -> [(Card, Play)]
getValidMoves round =
  case currentTurn round of
   Nothing -> []
   Just (pid, (dealt, hand)) ->
     [(dealt, play) | play <- getValidPlays pid otherPlayers dealt] ++
     [(hand, play) | play <- getValidPlays pid otherPlayers hand]
     where otherPlayers = delete pid $ getActivePlayers round
