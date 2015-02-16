{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haverer.Game (
  Game,
  Outcome,
  Score,
  finalScores,
  newGame,
  newRound,
  players,
  playersWon,
  scores,
  winners
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Random (MonadRandom)

import qualified Haverer.Counter as Counter
import Haverer.Deck (Deck, Complete, newDeck)
import Haverer.Player (
  PlayerId,
  PlayerSet,
  toPlayers,
  )
import Haverer.Prompt (ConsoleText, toText)
import Haverer.Round (Round)
import qualified Haverer.Round as Round


newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance ConsoleText Score where
  toText (Score n) = show n


type PlayerScores = Counter.Counter PlayerId Score

data Game = Game {
  _winningScore :: Score,
  _players :: Counter.Counter PlayerId Score,
  _playerSet :: PlayerSet
  } deriving Show


data Outcome = Outcome { _unoutcome :: (Counter.Counter PlayerId Score) }
               deriving Show


-- | Create a new game for the given set of players.
newGame :: PlayerSet -> Game
newGame ps = Game {
  _winningScore = Score 4, -- XXX: in some rule sets, this varies based on the number of players
  _players = (Counter.initialize $ toPlayers ps),
  _playerSet = ps
  }

-- | Start a new round of the game with an already-shuffled deck of cards.
newRound' :: Game -> Deck Complete -> Round
newRound' game deck = Round.newRound deck (_playerSet game)

-- | Start a new round of the game, shuffling the deck cards ourselves.
newRound :: (Functor m, MonadRandom m) => Game -> m Round
newRound game = newRound' game <$> newDeck

-- | Indicate that the specified players won.
playersWon :: Game -> [PlayerId] -> Either Outcome Game
playersWon game ps = onCounter game (flip Counter.incrementMany ps)

-- | Return the current scores of all the players.
scores :: Game -> [(PlayerId, Score)]
scores = Counter.toList . _players

-- | Return the set of all players
players :: Game -> PlayerSet
players = _playerSet

-- | Get the winners of the game.
winners :: Outcome -> [PlayerId]
winners = snd . Counter.top . _unoutcome

-- | Get the final scores at the end of the game.
finalScores :: Outcome -> [(PlayerId, Score)]
finalScores = Counter.toList . _unoutcome


-- | Perform an operation on the score counter. If this ends the game, return
-- a Left Outcome.
onCounter :: Game -> (PlayerScores -> PlayerScores) -> Either Outcome Game
onCounter game f =
  let players' = f $ _players game in
  if Counter.topValue players' >= _winningScore game
  then Left $ Outcome players'
  else Right $ game { _players = players' }
