{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haverer.Round where

import qualified Haverer.Counter as Counter
import Haverer.Player


newtype Score = Score Int deriving (Eq, Ord, Show, Num)

zeroScore :: Score
zeroScore = Score 0


data Game = Game {
  _players :: Counter.Counter PlayerId Score
  }

newGame :: PlayerSet -> Game
newGame ps = Game { _players = (Counter.initialize $ toPlayers ps) }

newGameForPlayers :: Int -> Maybe Game
newGameForPlayers = fmap newGame . makePlayerSet

playerWon :: Game -> PlayerId -> Game
playerWon game player =
  Game $ Counter.increment (_players game) player

playersWon :: Game -> [PlayerId] -> Game
playersWon game ps = Game $ Counter.incrementMany (_players game) ps

scores :: Game -> [(PlayerId, Score)]
scores = Counter.toList . _players

players :: Game -> [PlayerId]
players = fst . unzip . Counter.toList . _players

winning :: Game -> (Score, [PlayerId])
winning = Counter.top . _players
