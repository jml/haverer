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

module Haverer.PlayerSet (
  PlayerSet
  , PlayerSetError(..)
  , toPlayerSet
  , toPlayers
  , randomize
  , rotate
  ) where


import BasicPrelude
import Control.Monad.Except
import Control.Monad.Random
import System.Random.Shuffle


data PlayerSetError a = InvalidNumPlayers Int | DuplicatePlayers [a] deriving (Show, Eq)


newtype PlayerSet a = PlayerSet { toPlayers :: [a] } deriving (Show, Eq)


toPlayerSet :: Ord a => [a] -> Either (PlayerSetError a) (PlayerSet a)
toPlayerSet playerIds
  | numPlayers /= numDistinctPlayers = throwError (DuplicatePlayers playerIds)
  | numPlayers < 2 || numPlayers > 4 = throwError (InvalidNumPlayers numPlayers)
  | otherwise = (return . PlayerSet) playerIds
  where numPlayers = length playerIds
        numDistinctPlayers = (length . nub . sort) playerIds


-- | Rotate the order of the PlayerSet
--
-- The player who was first is now last, whoever was second is now third,
-- whoever was third is now second, etc.
--
-- Since 0.3
rotate :: PlayerSet a -> PlayerSet a
rotate (PlayerSet (x:xs)) = PlayerSet (xs ++ [x])
rotate _ = error "Empty PlayerSet is impossible"


-- | Randomize the order of the PlayerSet
--
-- Since 0.3
randomize :: MonadRandom m => PlayerSet a -> m (PlayerSet a)
randomize = map PlayerSet . shuffleM . toPlayers

