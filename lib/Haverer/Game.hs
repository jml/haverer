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


module Haverer.Game (
  Game,
  Outcome,
  finalScores,
  makeGame,
  newRound,
  newRound',
  players,
  playersWon,
  roundsPlayed,
  scores,
  winners
  ) where

import BasicPrelude
import Control.Monad.Random (MonadRandom)

import Haverer.Deck (FullDeck, newDeck)
import Haverer.PlayerSet (
  PlayerSet,
  rotate,
  toPlayers,
  )
import Haverer.Round (Round)
import qualified Haverer.Round as Round

import qualified Haverer.Internal.Counter as Counter


type PlayerScores a = Counter.Counter a Int


data Game playerId = Game {
  _winningScore :: Int,
  _players :: PlayerScores playerId,
  _playerSet :: PlayerSet playerId,
  _roundsPlayed :: Int
  } deriving Show


data Outcome playerId = Outcome { _unoutcome :: PlayerScores playerId } deriving Show


-- | Create a new game for the given set of players.
makeGame :: Ord playerId => PlayerSet playerId -> Game playerId
makeGame ps = Game {
  _winningScore = 4, -- XXX: in some rule sets, this varies based on the number of players
  _players = Counter.initialize $ toPlayers ps,
  _playerSet = ps,
  _roundsPlayed = 0
  }

-- | Start a new round of the game with an already-shuffled deck of cards.
newRound' :: (Ord playerId, Show playerId) => Game playerId -> FullDeck -> Round playerId
newRound' game deck = Round.makeRound deck (_playerSet game)

-- | Start a new round of the game, shuffling the deck cards ourselves.
newRound :: (Functor m, MonadRandom m, Ord playerId, Show playerId) => Game playerId -> m (Round playerId)
newRound game = newRound' game <$> newDeck

-- | Indicate that the specified players won.
--
-- Since 0.3, will also rotate the order of play, so the person to the left of
-- the previous first person is now the first.
playersWon :: Ord playerId => Game playerId -> [playerId] -> Either (Outcome playerId) (Game playerId)
playersWon game ps =
  rotatePlayers . bumpRoundsPlayed <$> onCounter game (`Counter.incrementMany` ps)
  where
    bumpRoundsPlayed g = g { _roundsPlayed = _roundsPlayed g + 1 }
    rotatePlayers g = g { _playerSet = rotate (_playerSet g) }

-- | Return the number of rounds played.
roundsPlayed :: Game playerId -> Int
roundsPlayed = _roundsPlayed

-- | Return the current scores of all the players.
scores :: Ord playerId => Game playerId -> [(playerId, Int)]
scores = Counter.toList . _players

-- | Return the set of all players
players :: Game playerId -> PlayerSet playerId
players = _playerSet

-- | Get the winners of the game.
winners :: Ord playerId => Outcome playerId -> [playerId]
winners = snd . Counter.top . _unoutcome

-- | Get the final scores at the end of the game.
finalScores :: Ord playerId => Outcome playerId -> [(playerId, Int)]
finalScores = Counter.toList . _unoutcome


-- | Perform an operation on the score counter. If this ends the game, return
-- a Left Outcome.
onCounter :: Ord playerId => Game playerId -> (PlayerScores playerId -> PlayerScores playerId) -> Either (Outcome playerId) (Game playerId)
onCounter game f =
  let players' = f $ _players game in
  if Counter.topValue players' >= _winningScore game
  then Left $ Outcome players'
  else Right $ game { _players = players' }
