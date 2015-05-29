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

{-# LANGUAGE NoImplicitPrelude #-}

module Haverer.ValidMoves (
  getValidMoves
  , protectedPlayers
  , movesThatTargetPlayer
  , attacksOnProtectedPlayers
  ) where

import BasicPrelude hiding (round)

import Haverer.Action (getTarget, getValidPlays, Play)
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
   Just (pid, (dealt, hand)) -> getValidPlays pid otherPlayers dealt hand
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
