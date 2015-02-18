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
import Haverer.Player (isProtected, PlayerId)
import Haverer.Round (
  Round,
  currentTurn,
  getActivePlayers,
  getPlayer
  )


getValidMoves :: Round -> [(Card, Play)]
getValidMoves round =
  case currentTurn round of
   Nothing -> []
   Just (pid, (dealt, hand))
     | bustingHand dealt hand -> []
     | otherwise ->
         [(dealt, play) | play <- getValidPlays pid otherPlayers dealt] ++
         [(hand, play) | play <- getValidPlays pid otherPlayers hand]
     where otherPlayers = delete pid $ getActivePlayers round


protectedPlayers :: Round -> [PlayerId]
protectedPlayers round =
  filter (\p -> Just True == (isProtected =<< getPlayer round p)) $ getActivePlayers round


movesThatTargetPlayer :: Round -> PlayerId -> [(Card, Play)]
movesThatTargetPlayer round target =
  filter ((== Just target) . getTarget . snd)  (getValidMoves round)


attacksOnProtectedPlayers :: Round -> [(Card, Play)]
attacksOnProtectedPlayers round =
  [(card, play) | p <- protectedPlayers round, (card, play) <- movesThatTargetPlayer round p]
