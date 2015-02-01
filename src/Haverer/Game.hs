{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haverer.Game where

import qualified Haverer.Counter as Counter
import Haverer.Player


newtype Score = Score Int deriving (Eq, Ord, Show, Num)

type PlayerScores = Counter.Counter PlayerId Score

data Game = Game {
  _winningScore :: Score,
  _players :: Counter.Counter PlayerId Score
  } deriving Show

newGame :: PlayerSet -> Game
newGame ps = Game {
  _winningScore = Score 4, -- XXX: in some rule sets, this varies based on the number of players
  _players = (Counter.initialize $ toPlayers ps)
  }

-- XXX: fclabels
onCounter :: Game -> (PlayerScores -> PlayerScores) -> Maybe Game
onCounter game f =
  if topScoreReached game
  then Nothing
  else Just $ game { _players = (f $ _players game) }

newGameForPlayers :: Int -> Maybe Game
newGameForPlayers = fmap newGame . makePlayerSet

playerWon :: Game -> PlayerId -> Maybe Game
playerWon game player = onCounter game (flip Counter.increment player)

playersWon :: Game -> [PlayerId] -> Maybe Game
playersWon game ps = onCounter game (flip Counter.incrementMany ps)

scores :: Game -> [(PlayerId, Score)]
scores = Counter.toList . _players

players :: Game -> [PlayerId]
players = fst . unzip . Counter.toList . _players

winning :: Game -> (Score, [PlayerId])
winning = Counter.top . _players

topScoreReached :: Game -> Bool
topScoreReached game = Counter.topValue (_players game) >= _winningScore game

winners :: Game -> Maybe [PlayerId]
winners game = if topScoreReached game then (Just . snd . winning) game else Nothing

