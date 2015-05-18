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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Round where

import BasicPrelude hiding (round)

import Data.Maybe (fromJust)

import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..), bustingHand, getTarget)
import Haverer.Deck (Card)
import Haverer.Player (getHand, getDiscards, isProtected)
import Haverer.Round (
  Round
  , Event(NoChange)
  , Result(..)
  , currentPlayer
  , currentTurn
  , getActivePlayers
  , getPlayer
  , nextPlayer
  , playTurn
  , prop_allCardsPresent
  , prop_burnCardsSame
  , prop_multipleActivePlayers
  , prop_ringIsActivePlayers
  )
import Haverer.ValidMoves (attacksOnProtectedPlayers)

import Haverer.Testing

import Utils (isSubListOf)

-- | It is impossible for the next player to be the current player. That would
-- mean there's only one person still playing, which would mean that the round
-- is over.
prop_nextPlayerNeverCurrentPlayer :: Round PlayerId -> Bool
prop_nextPlayerNeverCurrentPlayer round =
  currentPlayer round /= nextPlayer round || isNothing (currentPlayer round)


-- | The current player is *never* protected. At the start of your turn, any
-- protection you had expires.
prop_currentPlayerNeverProtected :: Round PlayerId -> Bool
prop_currentPlayerNeverProtected round =
  case currentPlayer round >>= getPlayer round >>= isProtected of
   Nothing -> True  -- round is over
   Just protected -> not protected


-- | Once inactive, you stay inactive. Takes a list of rounds that are
-- presumed to be consecutive and shows that each one has a larger list of
-- inactive players.
prop_inactivePlayersRemainSo :: [Round PlayerId] -> Bool
prop_inactivePlayersRemainSo round =
  let actives = fmap getActivePlayers round in
  and [isSubListOf y x | (x, y) <- zip actives (tail actives)]


-- | The given player is "the same" in two consecutive rounds. If the player is the
-- current player in the second round, then we ignore their 'protected'
-- status, since no player is ever protected on their first round.
prop_playerSame :: PlayerId -> Round PlayerId -> Round PlayerId -> Bool
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
prop_protectedUnaffected :: Round PlayerId -> Card -> Play PlayerId -> Property
prop_protectedUnaffected round card play =
  let target = getTarget play
      targetPlayer = getPlayer round =<< target
  in
   Just True == (isProtected =<< targetPlayer) ==>
   let (Played _ result, round') = playTurn' round card play in
    prop_playerSame (fromJust target) round round' &&
    (result == NoChange)


roundIsBusted :: Ord a => Round a -> Bool
roundIsBusted round =
  case currentTurn round of
   Nothing -> False
   Just (_, (c1, c2)) -> bustingHand c1 c2


genAttacksOnProtectedPlayers :: Gen (Round PlayerId, Card, Play PlayerId)
genAttacksOnProtectedPlayers = do
  -- XXX: Can I express this without calling attacksOnProtectedPlayers twice?
  round <- randomRound `suchThat` (not . null . attacksOnProtectedPlayers)
  (card, play) <- elements $ attacksOnProtectedPlayers round
  return (round, card, play)


-- | If you've got a busting hand, then no matter what you play, you're going
-- to lose.
prop_ministerBustsOut :: Round PlayerId -> Property
prop_ministerBustsOut round =
  let Just (pid, (dealt, hand)) = currentTurn round in
  bustingHand dealt hand ==>
  let Left (Right (event, round')) = playTurn round in
   pid `notElem` getActivePlayers round' &&
   event == BustedOut pid dealt hand


suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
  [ testProperty "allCardsPresent" (prop_allCardsPresent :: Round PlayerId -> Bool)
  , testProperty "allCardsPresent after many moves" $
    forAll randomRounds $ all prop_allCardsPresent
  , testProperty "next player is not current player" prop_nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after many moves" $
    forAll randomRounds $ all prop_nextPlayerNeverCurrentPlayer
  , testProperty "ring is active players" (prop_ringIsActivePlayers :: Round PlayerId -> Bool)
  , testProperty "ring is active players after move" $
    forAll randomRounds $ all prop_ringIsActivePlayers
  , testProperty "burn card same after move" $
    forAll randomRounds prop_burnCardsSame
  , testProperty "multiple active players or over after many moves" $
    forAll randomRounds $ all prop_multipleActivePlayers
  , testProperty "once deactivated stay that way" $
    forAll randomRounds prop_inactivePlayersRemainSo
  , testProperty "never protected on your turn" $
    forAll randomRounds $ all prop_currentPlayerNeverProtected
  , testProperty "attacks on protected never succeed" $
    forAll genAttacksOnProtectedPlayers $ \(r, c, p) -> prop_protectedUnaffected r c p
  , testProperty "minister + high card deactivates player" $
    forAll (randomRound `suchThat` roundIsBusted) prop_ministerBustsOut
  ]
 ]
