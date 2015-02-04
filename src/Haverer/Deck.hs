module Haverer.Deck (
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


import Control.Monad
import Control.Monad.Random (MonadRandom)
import Data.List (sort, (\\))
import System.Random.Shuffle (shuffleM)

data Card = Soldier | Clown | Knight | Priestess | Wizard | General | Minister | Prince
          deriving (Eq, Show, Ord)


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

validateSubDeck :: [Card] -> Maybe (Deck Incomplete)
validateSubDeck cards = if null $ cards \\ baseCards then Just (Deck cards) else Nothing

toList :: Deck a -> [Card]
toList (Deck xs) = xs
