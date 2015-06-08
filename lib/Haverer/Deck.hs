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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Haverer.Deck (
  allCards,
  baseCards,
  Card(..),
  DeckSize(..),
  deal,
  Deck,
  FullDeck,
  makeDeck,
  newDeck,
  pop,
  shuffleDeck,
  toList
  ) where


import BasicPrelude

import Control.Monad.Random (MonadRandom)
import System.Random.Shuffle (shuffleM)


data Card = Soldier | Clown | Knight | Priestess | Wizard | General | Minister | Prince
          deriving (Eq, Show, Ord, Enum)


allCards :: [Card]
allCards = [Soldier ..]


data DeckSize = Incomplete | Complete

newtype Deck (a :: DeckSize) = Deck [Card] deriving (Eq, Show, Ord)

type FullDeck = Deck 'Complete


baseCards :: [Card]
baseCards = [
  Soldier
  , Soldier
  , Soldier
  , Soldier
  , Soldier
  , Clown
  , Clown
  , Knight
  , Knight
  , Priestess
  , Priestess
  , Wizard
  , Wizard
  , General
  , Minister
  , Prince
  ]

baseDeck :: Deck 'Complete
baseDeck = Deck baseCards

shuffleDeck :: MonadRandom m => Deck a -> m (Deck a)
shuffleDeck (Deck d) = liftM Deck $ shuffleM d

newDeck :: MonadRandom m => m (Deck 'Complete)
newDeck = shuffleDeck baseDeck

makeDeck :: [Card] -> Maybe (Deck 'Complete)
makeDeck cards =
  if sort cards == baseCards then Just (Deck cards) else Nothing

pop :: Deck a -> (Maybe Card, Deck 'Incomplete)
pop (Deck []) = (Nothing, Deck [])
pop (Deck (c:cards)) = (Just c, Deck cards)

deal :: Deck a -> Int -> (Maybe [Card], Deck 'Incomplete)
deal (Deck cards) n =
  case splitAt n cards of
   (_, []) -> (Nothing, Deck cards)
   (top, rest) -> (Just top, Deck rest)

toList :: Deck a -> [Card]
toList (Deck xs) = xs
