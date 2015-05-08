{-# LANGUAGE MultiParamTypeClasses #-}

module Haverer.Engine (
  MonadEngine(..)
  , playGame
  ) where

import Prelude hiding (round)

import Control.Monad.Random (MonadRandom)
import Data.Maybe (fromJust)

import Haverer.Action (Play)
import Haverer.Deck (Card)
import qualified Haverer.Game as Game
import qualified Haverer.Round as Round
import Haverer.Player (PlayerSet)


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


playGame :: (Ord playerId, Show playerId, Functor m, MonadRandom m, MonadEngine m playerId) => PlayerSet playerId -> m (Game.Outcome playerId)
playGame players = do
  let game = Game.makeGame players
  gameStarted game
  outcome <- playGame' game
  gameOver outcome
  return outcome


playGame' :: (Show playerId, Ord playerId, Functor m, MonadRandom m, MonadEngine m playerId) => Game.Game playerId -> m (Game.Outcome playerId)
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
     (round', event) <- getPlay players r player dealt hand
     handOver event
     return $ Just round'


getPlay :: (Ord playerId, Show playerId, MonadEngine m playerId) => PlayerSet playerId -> Round.Round playerId -> playerId -> Card -> Card -> m (Round.Round playerId, Round.Result playerId)
getPlay players round player dealt hand =
  case Round.playTurn round of
   Left (round', event) -> return (round', event)
   Right handlePlay -> do
     (card, play) <- choosePlay players player dealt hand
     case handlePlay card play of
      Left err -> do
        badPlay err
        getPlay players round player dealt hand
      Right (round', event) -> return (round', event)
