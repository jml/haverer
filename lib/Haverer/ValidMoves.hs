module Haverer.ValidMoves (
  getValidMoves
  , protectedPlayers
  , movesThatTargetPlayer
  , attacksOnProtectedPlayers
  ) where

import Data.List (delete)
import Prelude hiding (round)

import Haverer.Action (bustingHand, getTarget, getValidPlays, Play)
import Haverer.Deck (Card)
import Haverer.Player (isProtected)
import Haverer.Round (
  Round,
  currentTurn,
  getActivePlayers,
  getPlayer
  )


getValidMoves :: Ord playerId => Round playerId -> [(Card, Play playerId)]
getValidMoves round =
  case currentTurn round of
   Nothing -> []
   Just (pid, (dealt, hand))
     | bustingHand dealt hand -> []
     | otherwise ->
         [(dealt, play) | play <- getValidPlays pid otherPlayers dealt] ++
         [(hand, play) | play <- getValidPlays pid otherPlayers hand]
     where otherPlayers = delete pid $ getActivePlayers round


protectedPlayers :: Ord playerId => Round playerId -> [playerId]
protectedPlayers round =
  filter (\p -> Just True == (isProtected =<< getPlayer round p)) $ getActivePlayers round


movesThatTargetPlayer :: Ord playerId => Round playerId -> playerId -> [(Card, Play playerId)]
movesThatTargetPlayer round target =
  filter ((== Just target) . getTarget . snd)  (getValidMoves round)


attacksOnProtectedPlayers :: Ord playerId => Round playerId -> [(Card, Play playerId)]
attacksOnProtectedPlayers round =
  [(card, play) | p <- protectedPlayers round, (card, play) <- movesThatTargetPlayer round p]
