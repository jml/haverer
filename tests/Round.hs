{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Prelude hiding (round)

import Control.Applicative ((<*>), (<$>))
import Data.List (delete, sort)
import Data.Maybe (fromJust)

import System.Random.Shuffle (shuffle)
import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..))
import Haverer.Deck (baseCards, Card(..), Complete, Deck, makeDeck)
import Haverer.Player (isProtected, makePlayerSet, PlayerSet)
import Haverer.Round (
  BadAction
  , Round
  , currentPlayer
  , getActivePlayers
  , getPlayer
  , newRound
  , nextPlayer
  , playTurn
  , prop_allCardsPresent
  , prop_burnCardsSame
  , prop_multipleActivePlayers
  , prop_ringIsActivePlayers
  )
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
applyPlay r (Just (card, play)) = fst <$> playTurn r card play

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error $ "unexpected Left " ++ show a


randomNextMove :: Round -> Gen Round
randomNextMove r = fmap (fromRight . applyPlay r) (randomCardPlay r)


iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f = iterate (f =<<) . return


makeN' :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
makeN' n f x
  | n == 0 = return [x]
  | n > 0  = do y <- f x
                ys <- makeN' (n - 1) f y
                return (y:ys)
  | otherwise = return []


manyMoves :: Int -> Gen [Round]
manyMoves 0 = return []
manyMoves n = do
  initial <- arbitrary
  rest <- makeN' (n - 2) randomNextMove initial
  return (initial:rest)


randomRounds :: Gen [Round]
randomRounds = do
  num <- choose (2, 14)
  manyMoves num

nextPlayerNeverCurrentPlayer :: Round -> Bool
nextPlayerNeverCurrentPlayer round =
  currentPlayer round /= nextPlayer round || currentPlayer round == Nothing


prop_currentPlayerNeverProtected :: Round -> Bool
prop_currentPlayerNeverProtected round =
  case currentPlayer round >>= getPlayer round >>= isProtected of
   Nothing -> True  -- round is over
   Just protected -> not protected


isSubListOf :: (Eq a, Ord a) => [a] -> [a] -> Bool
isSubListOf xs ys = isSubListOf' (sort xs) (sort ys)

isSubListOf' :: Eq a => [a] -> [a] -> Bool
isSubListOf' (_:_) []  = False
isSubListOf' [] _      = True
isSubListOf' (x:xs) ys =
  if x `elem` ys
  then isSubListOf' xs (delete x ys)
  else False


prop_inactivePlayersRemainSo :: [Round] -> Bool
prop_inactivePlayersRemainSo round =
  let actives = fmap getActivePlayers round in
  and [isSubListOf y x | (x, y) <- zip actives (tail actives)]


suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
  [ testProperty "allCardsPresent" prop_allCardsPresent
  , testProperty "allCardsPresent after many moves" $
    forAll randomRounds $ all prop_allCardsPresent
  , testProperty "next player is not current player" nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after many moves" $
    forAll randomRounds $ all nextPlayerNeverCurrentPlayer
  , testProperty "ring is active players" $ prop_ringIsActivePlayers
  , testProperty "ring is active players after move" $
    forAll randomRounds $ all prop_ringIsActivePlayers
  , testProperty "burn card same after move" $
    forAll randomRounds $ prop_burnCardsSame
  , testProperty "multiple active players or over after many moves" $
    forAll randomRounds $ all prop_multipleActivePlayers
  , testProperty "once deactivated stay that way" $
    forAll randomRounds $ prop_inactivePlayersRemainSo
  , testProperty "never protected on your turn" $
    forAll randomRounds $ all prop_currentPlayerNeverProtected
  ]
 ]
