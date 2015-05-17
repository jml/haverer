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


module Haverer.Deck (
  allCards,
  baseCards,
  Card(..),
  Complete,
  deal,
  Deck,
  Incomplete,
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


data Complete
data Incomplete

newtype Deck a = Deck [Card] deriving (Eq, Show, Ord)

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

baseDeck :: Deck Complete
baseDeck = Deck baseCards

shuffleDeck :: MonadRandom m => Deck a -> m (Deck a)
shuffleDeck (Deck d) = liftM Deck $ shuffleM d

newDeck :: MonadRandom m => m (Deck Complete)
newDeck = shuffleDeck baseDeck

makeDeck :: [Card] -> Maybe (Deck Complete)
makeDeck cards =
  if sort cards == baseCards then Just (Deck cards) else Nothing

pop :: Deck a -> (Deck Incomplete, Maybe Card)
pop (Deck []) = (Deck [], Nothing)
pop (Deck (c:cards)) = (Deck cards, Just c)

deal :: Deck a -> Int -> (Deck Incomplete, Maybe [Card])
deal (Deck cards) n =
  case splitAt n cards of
   (_, []) -> (Deck cards, Nothing)
   (top, rest) -> (Deck rest, Just top)

toList :: Deck a -> [Card]
toList (Deck xs) = xs
