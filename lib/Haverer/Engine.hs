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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Haverer.Engine (
  MonadEngine(..)
  , playGame
  ) where

import BasicPrelude hiding (round)

import Control.Monad.Random (MonadRandom)
import Data.Maybe (fromJust)

import Haverer.Action (Play)
import Haverer.Deck (Card)
import qualified Haverer.Game as Game
import qualified Haverer.Round as Round
import Haverer.PlayerSet (PlayerSet)


class Monad m => MonadEngine m playerId where

  badPlay :: Round.BadAction playerId -> m ()
  badPlay _ = return ()

  -- XXX: We're passing PlayerSet around everywhere just so we can have it
  -- here.
  choosePlay :: PlayerSet playerId -> playerId -> Card -> Card -> m (Card, Play playerId)

  gameStarted :: Game.Game playerId -> m ()
  gameStarted _ = return ()

  gameOver :: Game.Outcome playerId -> m ()
  gameOver _ = return ()

  roundStarted :: Game.Game playerId -> Round.Round playerId -> m ()
  roundStarted _ _ = return ()

  roundOver :: Round.Victory playerId -> m ()
  roundOver _ = return ()

  handStarted :: Round.Round playerId -> m ()
  handStarted _ = return ()

  handOver :: Round.Result playerId -> m ()
  handOver _ = return ()


playGame :: (Ord playerId, Show playerId, MonadRandom m, MonadEngine m playerId) => PlayerSet playerId -> m (Game.Outcome playerId)
playGame players = do
  let game = Game.makeGame players
  gameStarted game
  outcome <- playGame' game
  gameOver outcome
  return outcome


playGame' :: (Show playerId, Ord playerId, MonadRandom m, MonadEngine m playerId) => Game.Game playerId -> m (Game.Outcome playerId)
playGame' game = do
  round <- Game.newRound game
  roundStarted game round
  outcome <- playRound (Game.players game) round
  roundOver outcome
  case Game.playersWon game (Round.getWinners outcome) of
   Left o -> return o
   Right game' -> playGame' game'


playRound :: (Show playerId, Ord playerId, MonadEngine m playerId) => PlayerSet playerId -> Round.Round playerId -> m (Round.Victory playerId)
playRound players round = do
  result <- playHand players round
  case result of
   Just round' -> playRound players round'
   Nothing -> return $ fromJust $ Round.victory round


playHand :: (Show playerId, Ord playerId, MonadEngine m playerId) => PlayerSet playerId -> Round.Round playerId -> m (Maybe (Round.Round playerId))
playHand players r =
  case Round.currentTurn r of
   Nothing -> return Nothing
   Just (player, (dealt, hand)) -> do
     handStarted r
     (event, round') <- getPlay players r player dealt hand
     handOver event
     return $ Just round'


getPlay :: (Ord playerId, Show playerId, MonadEngine m playerId) => PlayerSet playerId -> Round.Round playerId -> playerId -> Card -> Card -> m (Round.Result playerId, Round.Round playerId)
getPlay players round player dealt hand = do
  result <- case Round.playTurn round of
             Left r -> return r
             Right handlePlay -> do
               (card, play) <- choosePlay players player dealt hand
               return $ handlePlay card play
  case result of
   Left err -> do
     badPlay err
     getPlay players round player dealt hand
   Right (event, round') -> return (event, round')
