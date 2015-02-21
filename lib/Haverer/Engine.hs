module Haverer.Engine (
  MonadEngine
  , playGame
  , badPlay
  , choosePlay
  , gameStarted
  , gameOver
  , handStarted
  , handOver
  , roundStarted
  , roundOver
  ) where

import Prelude hiding (round)

import Control.Monad.Random (MonadRandom)
import Data.Maybe (fromJust)

import Haverer.Action (Play)
import Haverer.Deck (Card)
import qualified Haverer.Game as Game
import qualified Haverer.Round as Round
import Haverer.Player (PlayerId, PlayerSet)


class Monad m => MonadEngine m where

  badPlay :: Round.BadAction -> m ()
  badPlay _ = return ()

  -- XXX: We're passing PlayerSet around everywhere just so we can have it
  -- here.
  choosePlay :: PlayerSet -> PlayerId -> Card -> Card -> m (Card, Play)

  gameStarted :: Game.Game -> m ()
  gameStarted _ = return ()

  gameOver :: Game.Outcome -> m ()
  gameOver _ = return ()

  roundStarted :: Game.Game -> Round.Round -> m ()
  roundStarted _ _ = return ()

  roundOver :: Round.Victory -> m ()
  roundOver _ = return ()

  handStarted :: Round.Round -> m ()
  handStarted _ = return ()

  handOver :: Round.Event -> m ()
  handOver _ = return ()


playGame :: (Functor m, MonadRandom m, MonadEngine m) => PlayerSet -> m Game.Outcome
playGame players = do
  let game = Game.makeGame players
  gameStarted game
  outcome <- playGame' game
  gameOver outcome
  return outcome


playGame' :: (Functor m, MonadRandom m, MonadEngine m) => Game.Game -> m Game.Outcome
playGame' game = do
  round <- Game.newRound game
  roundStarted game round
  outcome <- playRound (Game.players game) round
  roundOver outcome
  case Game.playersWon game (Round.getWinners outcome) of
   Left o -> return o
   Right game' -> playGame' game'


playRound :: MonadEngine m => PlayerSet -> Round.Round -> m Round.Victory
playRound players round = do
  result <- playHand players round
  case result of
   Just round' -> do
     playRound players round'
   Nothing -> return $ fromJust $ Round.victory round


playHand :: MonadEngine m => PlayerSet -> Round.Round -> m (Maybe Round.Round)
playHand players r =
  case Round.currentTurn r of
   Nothing -> return Nothing
   Just (player, (dealt, hand)) -> do
     handStarted r
     (round', event) <- getPlay players r player dealt hand
     handOver event
     return $ Just round'


getPlay :: MonadEngine m => PlayerSet -> Round.Round -> PlayerId -> Card -> Card -> m (Round.Round, Round.Event)
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
