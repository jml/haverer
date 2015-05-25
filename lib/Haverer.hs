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

{-| Implementation of the rules of Love Letter. -}

{-# LANGUAGE NoImplicitPrelude #-}

module Haverer (
  Game,
  makeGame,
  newRound,
  newRound',
  players,
  roundsPlayed,
  finalScores,
  scores,
  winners,
  Round,
  BadAction,
  Result(..),
  Event(..),
  playTurn,
  playTurn',
  getPlayers,
  getPlayerMap,
  currentPlayer,
  currentTurn,
  Card(..),
  Deck,
  Complete,
  newDeck,
  Play(..),
  viewAction,
  Player,
  getDiscards,
  getHand,
  isProtected,
  toPlayers,
  toPlayerSet,
  ) where


import Haverer.Action (Play(..), viewAction)
import Haverer.Deck (Card(..), Complete, Deck, newDeck)
import Haverer.Game (
  Game,
  finalScores,
  makeGame,
  newRound,
  newRound',
  players,
  roundsPlayed,
  scores,
  winners)
import Haverer.Player (
  Player,
  getDiscards,
  getHand,
  isProtected,
  toPlayers,
  toPlayerSet)
import Haverer.Round (
  BadAction,
  Result(..),
  Round,
  Event(..),
  currentPlayer,
  currentTurn,
  getPlayers,
  getPlayerMap,
  playTurn,
  playTurn')
