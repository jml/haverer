
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle

data Card = Soldier | Clown | Knight | Priestess | Wizard | General | Minister | Prince
          deriving (Eq, Show, Ord)


newtype Deck = Deck [Card] deriving (Eq, Show, Ord)

baseDeck :: Deck
baseDeck = Deck [
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


shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck (Deck d) = liftM Deck $ shuffleM d

newDeck :: MonadRandom m => m Deck
newDeck = shuffleDeck baseDeck


main :: IO ()
main = do
  d <- newDeck
  putStrLn $ show d
