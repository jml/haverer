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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Prelude hiding (round)

import Control.Applicative ((<*>), (<$>))
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..), bustingHand, getTarget)
import Haverer.Deck (baseCards, Card(..), Complete, Deck, makeDeck)
import Haverer.Player (getHand, getDiscards, isProtected, makePlayerSet, PlayerId, PlayerSet)
import Haverer.Prompt (toText)
import Haverer.Round (
  Round
  , Event(..)
  , Result(NothingHappened)
  , currentPlayer
  , currentTurn
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
import Haverer.ValidMoves (attacksOnProtectedPlayers, getValidMoves)

import Utils (shuffled, iterateM', isSubListOf)


instance Arbitrary (Deck Complete) where
  -- | An arbitrary complete deck is a shuffled set of cards.
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


instance Arbitrary PlayerSet where
  -- | Start the game with a random number of players.
  arbitrary = fmap (fromJust . makePlayerSet) (elements [2, 3, 4])


instance Arbitrary Round where
  -- | A fresh, unplayed round with an arbitrary number of players and a
  -- shuffled deck.
  arbitrary = newRound <$> arbitrary <*> arbitrary


-- | For a Round and a known-good Card and Play, play the cards and return the
-- round and event. If the hand busts out, Card and Play are ignored.
playTurn' :: Round -> Card -> Play -> (Round, Event)
playTurn' round card play =
  case playTurn round of
   Left (round', event) -> (round', event)
   Right handlePlay ->
     case handlePlay card play of
      Left err -> error $ "Should have generated valid play: " ++ show (err, round, card, play)
      Right (round', event) -> (round', event)


playRandomTurn :: Round -> Gen (Round, Event)
playRandomTurn round = do
  move <- randomCardPlay round
  case move of
   Nothing -> return (round, RoundOver)
   Just (card, play) -> return $ playTurn' round card play
  where
    randomCardPlay round' =
      case getValidMoves round' of
       [] -> return Nothing
       xs -> elements (fmap Just xs)


-- | Given a Round, generate a Round that's randomly had a move applied, i.e.
-- a possible next Round. If there are no valid moves, then return the same
-- Round.
randomNextMove :: Round -> Gen Round
randomNextMove round = fst <$> playRandomTurn round


-- | Generate a sequence of N rounds, starting from an initial round.
manyMoves :: Int -> Gen [Round]
manyMoves 0 = return []
manyMoves n = do
  initial <- arbitrary
  rest <- iterateM' (n - 2) randomNextMove initial
  return (initial:rest)


-- | Generate a random number of consecutive rounds, starting from an initial round.
randomRounds :: Gen [Round]
randomRounds = do
  num <- choose (2, 14)
  manyMoves num


-- | Generate a random round that might come up in the course of play.
randomRound :: Gen Round
randomRound = last <$> randomRounds


-- | Generate a random, non-terminating round and a valid play for that round.
roundAndPlay :: Gen (Round, Card, Play)
roundAndPlay = do
  round <- randomRound `suchThat` (not . null . getValidMoves)
  (card, play) <- elements $ getValidMoves round
  return (round, card, play)


-- | Generate an event that might come up in the course of play.
inRoundEvent :: Gen Event
inRoundEvent = do
  (round, card, play) <- roundAndPlay
  return $ snd $ playTurn' round card play


-- | It is impossible for the next player to be the current player. That would
-- mean there's only one person still playing, which would mean that the round
-- is over.
prop_nextPlayerNeverCurrentPlayer :: Round -> Bool
prop_nextPlayerNeverCurrentPlayer round =
  currentPlayer round /= nextPlayer round || currentPlayer round == Nothing


-- | The current player is *never* protected. At the start of your turn, any
-- protection you had expires.
prop_currentPlayerNeverProtected :: Round -> Bool
prop_currentPlayerNeverProtected round =
  case currentPlayer round >>= getPlayer round >>= isProtected of
   Nothing -> True  -- round is over
   Just protected -> not protected


-- | Once inactive, you stay inactive. Takes a list of rounds that are
-- presumed to be consecutive and shows that each one has a larger list of
-- inactive players.
prop_inactivePlayersRemainSo :: [Round] -> Bool
prop_inactivePlayersRemainSo round =
  let actives = fmap getActivePlayers round in
  and [isSubListOf y x | (x, y) <- zip actives (tail actives)]


-- | The given player is "the same" in two consecutive rounds. If the player is the
-- current player in the second round, then we ignore their 'protected'
-- status, since no player is ever protected on their first round.
prop_playerSame :: PlayerId -> Round -> Round -> Bool
prop_playerSame pid round round' =
  let player = getPlayer round pid
      player' = getPlayer round' pid in
   case currentPlayer round' of
    Just p | p == pid ->
               (getHand =<< player) == (getHand =<< player') &&
               (getDiscards <$> player) == (getDiscards <$> player')
    _ -> player == player'


-- | Attacking a player who is protected always leaves it in the exact same
-- state when the turn is done. The only thing that might change is that it
-- might now be the attacked player's turn.
prop_protectedUnaffected :: Round -> Card -> Play -> Property
prop_protectedUnaffected round card play =
  let target = getTarget play
      targetPlayer = getPlayer round =<< target
  in
   Just True == (isProtected =<< targetPlayer) ==>
   let (round', Played _ result) = playTurn' round card play in
    prop_playerSame (fromJust target) round round' &&
    (result == NothingHappened)


roundIsBusted :: Round -> Bool
roundIsBusted round =
  case currentTurn round of
   Nothing -> False
   Just (_, (c1, c2)) -> bustingHand c1 c2


genAttacksOnProtectedPlayers :: Gen (Round, Card, Play)
genAttacksOnProtectedPlayers = do
  -- XXX: Can I express this without calling attacksOnProtectedPlayers twice?
  round <- randomRound `suchThat` (not . null . attacksOnProtectedPlayers)
  (card, play) <- elements $ attacksOnProtectedPlayers round
  return (round, card, play)


-- | If you've got a busting hand, then no matter what you play, you're going
-- to lose.
prop_ministerBustsOut :: Round -> Property
prop_ministerBustsOut round =
  let Just (pid, (dealt, hand)) = currentTurn round in
  bustingHand dealt hand ==>
  let Left (round', event) = playTurn round in
   not (pid `elem` getActivePlayers round') &&
   event == BustedOut pid dealt hand


suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
  [ testProperty "allCardsPresent" prop_allCardsPresent
  , testProperty "allCardsPresent after many moves" $
    forAll randomRounds $ all prop_allCardsPresent
  , testProperty "next player is not current player" prop_nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after many moves" $
    forAll randomRounds $ all prop_nextPlayerNeverCurrentPlayer
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
  , testProperty "attacks on protected never succeed" $
    forAll genAttacksOnProtectedPlayers $ \(r, c, p) -> prop_protectedUnaffected r c p
  , testProperty "minister + high card deactivates player" $
    forAll (randomRound `suchThat` roundIsBusted) $ prop_ministerBustsOut
  , testProperty "event toText coverage" $
    forAll inRoundEvent $ not . isPrefixOf "UNKNOWN" . toText
  ]
 ]
