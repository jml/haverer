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

{-# LANGUAGE TemplateHaskell #-}

module Haverer.Player (
  bust,
  Error,
  discardAndDraw,
  eliminate,
  getDiscards,
  getHand,
  isProtected,
  makePlayerSet,
  makePlayer,
  Player,
  PlayerId,
  PlayerSet,
  playCard,
  protect,
  swapHands,
  toPlayers,
  unprotect
  ) where

import Control.Lens hiding (chosen)
import Data.List (nub, sort)

import Haverer.Deck (Card)


newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)

players :: [PlayerId]
players = map PlayerId [1..]


data Error = InvalidNumPlayers Int | DuplicatePlayers


newtype PlayerSet = PlayerSet [PlayerId] deriving (Show)


toPlayers :: PlayerSet -> [PlayerId]
toPlayers (PlayerSet xs) = xs

toPlayerSet :: [PlayerId] -> Either Error PlayerSet
toPlayerSet playerIds =
  if playerIds /= (nub . sort) playerIds
  then Left DuplicatePlayers
       else if numPlayers < 2 || numPlayers > 4
            then Left (InvalidNumPlayers numPlayers)
            else (Right . PlayerSet) playerIds
  where numPlayers = length playerIds


makePlayerSet :: Int -> Maybe PlayerSet
makePlayerSet n =
  case toPlayerSet (take n players) of
   Left _ -> Nothing
   Right playerIds -> Just playerIds


data Player = Active {
  _hand :: Card,
  _protected :: Bool,
  _discard :: [Card]
  } | Inactive { _discard :: [Card] } deriving (Show, Eq)

makeLenses ''Player

makePlayer :: Card -> Player
makePlayer card = Active {
  _hand = card,
  _protected = False,
  _discard = []
  }


operateOn :: (Player -> Player) -> Player -> Maybe Player
operateOn _ (Inactive _) = Nothing
operateOn _ player@(Active _ True _) = Just player
operateOn f player = Just $ f player


protect :: Player -> Maybe Player
protect = _setProtect True

unprotect :: Player -> Maybe Player
unprotect = _setProtect False

_setProtect :: Bool -> Player -> Maybe Player
_setProtect _ (Inactive _) = Nothing
_setProtect p player = Just $ set protected p player



eliminate :: Player -> Maybe Player
eliminate = operateOn (\(Active card _ discards) -> Inactive (card:discards))

swapHands :: Player -> Player -> Maybe (Player, Player)
swapHands player1 player2 =
  case (player1, player2) of
   (Active h1 p _, Active h2 _ _) ->
     if p
     then Just (player1, player2)
     else Just (set hand h2 player1, set hand h1 player2)
   _ -> Nothing


-- Will not de-activate player if they discard a Prince.
discardAndDraw :: Player -> Maybe Card -> Maybe Player
discardAndDraw (Inactive _) _ = Nothing
discardAndDraw (Active card False discards) Nothing = Just $ Inactive (card:discards)
discardAndDraw player@(Active _ True _) _ = Just player
discardAndDraw (Active card p discards) (Just newCard) =
  Just $ Active newCard p (card:discards)


-- |Given a dealt and chosen card, update the hand to chosen, and chuck
-- whatever wasn't played onto the discard pile.
playCard :: Player -> Card -> Card -> Maybe Player
playCard (Inactive _) _ _ = Nothing
playCard (Active h p discards) dealt chosen =
  if h == chosen
  then Just $ Active dealt p (h:discards)
  else if dealt == chosen
       then Just $ Active h p (dealt:discards)
       else Nothing


bust :: Player -> Card -> Maybe Player
bust (Inactive _) _ = Nothing
bust (Active h _ discards) dealt = Just $ Inactive (h:dealt:discards)


getDiscards :: Player -> [Card]
getDiscards = view discard

getHand :: Player -> Maybe Card
getHand = preview hand

isProtected :: Player -> Maybe Bool
isProtected = preview protected
