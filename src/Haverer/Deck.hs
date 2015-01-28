module Haverer.Deck (
  Card(..),
  Deck,
  shuffleDeck,
  newDeck,
  ) where


import Control.Monad
import Control.Monad.Random (MonadRandom)
import Data.List (sort)
import System.Random.Shuffle (shuffleM)

data Card = Soldier | Clown | Knight | Priestess | Wizard | General | Minister | Prince
          deriving (Eq, Show, Ord)


newtype Deck = Deck [Card] deriving (Eq, Show, Ord)

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

baseDeck :: Deck
baseDeck = Deck baseCards


shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck (Deck d) = liftM Deck $ shuffleM d

newDeck :: MonadRandom m => m Deck
newDeck = shuffleDeck baseDeck

makeDeck :: [Card] -> Maybe Deck
makeDeck cards =
  if sort cards == baseCards then Just (Deck cards) else Nothing
