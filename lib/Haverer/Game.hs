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

module Haverer.Game (
  Game,
  Outcome,
  finalScores,
  makeGame,
  newRound,
  players,
  playersWon,
  roundsPlayed,
  scores,
  winners
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Random (MonadRandom)

import Haverer.Deck (Deck, Complete, newDeck)
import Haverer.Player (
  PlayerId,
  PlayerSet,
  toPlayers,
  )
import Haverer.Round (Round)
import qualified Haverer.Round as Round

import qualified Haverer.Internal.Counter as Counter


type PlayerScores = Counter.Counter PlayerId Int


data Game = Game {
  _winningScore :: Int,
  _players :: PlayerScores,
  _playerSet :: PlayerSet,
  _roundsPlayed :: Int
  } deriving Show


data Outcome = Outcome { _unoutcome :: PlayerScores } deriving Show


-- | Create a new game for the given set of players.
makeGame :: PlayerSet -> Game
makeGame ps = Game {
  _winningScore = 4, -- XXX: in some rule sets, this varies based on the number of players
  _players = (Counter.initialize $ toPlayers ps),
  _playerSet = ps,
  _roundsPlayed = 0
  }

-- | Start a new round of the game with an already-shuffled deck of cards.
newRound' :: Game -> Deck Complete -> Round
newRound' game deck = Round.makeRound deck (_playerSet game)

-- | Start a new round of the game, shuffling the deck cards ourselves.
newRound :: (Functor m, MonadRandom m) => Game -> m Round
newRound game = newRound' game <$> newDeck

-- | Indicate that the specified players won.
playersWon :: Game -> [PlayerId] -> Either Outcome Game
playersWon game ps =
  bumpRoundsPlayed <$> onCounter game (flip Counter.incrementMany ps)
  where
    bumpRoundsPlayed g = g { _roundsPlayed = (_roundsPlayed g) + 1 }

-- | Return the number of rounds played.
roundsPlayed :: Game -> Int
roundsPlayed = _roundsPlayed

-- | Return the current scores of all the players.
scores :: Game -> [(PlayerId, Int)]
scores = Counter.toList . _players

-- | Return the set of all players
players :: Game -> PlayerSet
players = _playerSet

-- | Get the winners of the game.
winners :: Outcome -> [PlayerId]
winners = snd . Counter.top . _unoutcome

-- | Get the final scores at the end of the game.
finalScores :: Outcome -> [(PlayerId, Int)]
finalScores = Counter.toList . _unoutcome


-- | Perform an operation on the score counter. If this ends the game, return
-- a Left Outcome.
onCounter :: Game -> (PlayerScores -> PlayerScores) -> Either Outcome Game
onCounter game f =
  let players' = f $ _players game in
  if Counter.topValue players' >= _winningScore game
  then Left $ Outcome players'
  else Right $ game { _players = players' }
