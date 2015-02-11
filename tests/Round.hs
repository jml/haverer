{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Prelude hiding (round)

import Control.Applicative ((<*>), (<$>))
import Data.Maybe (fromJust)

import System.Random.Shuffle (shuffle)
import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..))
import Haverer.Deck (baseCards, Card(..), Complete, Deck, makeDeck)
import Haverer.Player (isProtected, makePlayerSet, PlayerSet)
import Haverer.Round
import Haverer.ValidMoves (getValidMoves)


instance Arbitrary (Deck Complete) where
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


shuffled ::[a] -> Gen [a]
shuffled xs = do
  rs <- randomOrdering (length xs - 1)
  return $ shuffle xs rs
  where
    -- a sequence (r1,...r[n-1]) of numbers such that r[i] is an independent
    -- sample from a uniform random distribution [0..n-i]
    randomOrdering 0 = return []
    randomOrdering n =
      do y <- choose (0, n)
         ys <- randomOrdering (n - 1)
         return (y:ys)


-- TODO: Write a property for getValidMoves such that all valid moves return a
-- new, different Round, and not an Error.

instance Arbitrary PlayerSet where
  arbitrary = fmap (fromJust . makePlayerSet) (elements [2, 3, 4])

instance Arbitrary Round where
  arbitrary = newRound <$> arbitrary <*> arbitrary


randomCardPlay :: Round -> Gen (Maybe (Card, Play))
randomCardPlay round =
  case getValidMoves round of
   [] -> return Nothing
   xs -> elements (fmap Just xs)


applyPlay :: Round -> Maybe (Card, Play) -> Either BadAction Round
applyPlay r Nothing = return r
applyPlay r (Just (card, play)) = fmap fst (thingy r card play)

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error $ "unexpected Left " ++ show a


randomNextMove :: Round -> Gen Round
randomNextMove r = fmap (fromRight . applyPlay r) (randomCardPlay r)


iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f = iterate (f =<<) . return


nextPlayerNeverCurrentPlayer :: Round -> Bool
nextPlayerNeverCurrentPlayer round =
  currentPlayer round /= nextPlayer round || currentPlayer round == Nothing


-- FIXME: Priestess never expires.
prop_currentPlayerNeverProtected :: Round -> Bool
prop_currentPlayerNeverProtected round =
  case currentPlayer round >>= getPlayer round >>= isProtected of
   Nothing -> True  -- round is over
   Just protected -> not protected



-- XXX: Do random [0..15] for number of moves, so we exercise more of the
-- tree, then ditch 'after one move' bollocks.

suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
  [ testProperty "allCardsPresent" prop_allCardsPresent
  , testProperty "allCardsPresent after move" $
    forAll (arbitrary >>= randomNextMove) prop_allCardsPresent
  , testProperty "allCardsPresent after many moves" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all prop_allCardsPresent
  , testProperty "next player is not current player" nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after turn"
    $ forAll (arbitrary >>= randomNextMove) nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after many moves" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all nextPlayerNeverCurrentPlayer
  , testProperty "ring is active players" $ prop_ringIsActivePlayers
  , testProperty "ring is active players after move" $
    forAll (arbitrary >>= randomNextMove) prop_ringIsActivePlayers
  , testProperty "burn card same after move" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ prop_burnCardsSame
  , testProperty "multiple active players or over" $
    forAll (arbitrary >>= randomNextMove) prop_multipleActivePlayers
  , testProperty "multiple active players or over after many moves" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all prop_multipleActivePlayers

  , testProperty "never protected on your turn" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all prop_currentPlayerNeverProtected
  ]
 ]
