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

{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haverer.CLI.CommandLine (
  formatScores,
  pickNumPlayers,
  pickCardToPlay,
  pickPlay
  ) where

import Prelude hiding (round)

import Data.List (intercalate)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Haverer.Action (Play(..), viewAction)
import Haverer.Deck (Card(..))
import Haverer.Player (PlayerId, PlayerSet, getDiscards, isProtected, toPlayers)
import Haverer.Round (
  Event(..),
  Result(..),
  Round,
  Victory(..),
  getPlayerMap,
  remainingCards
  )

import Haverer.CLI.Prompt (
  ConsoleText,
  repeatedlyPrompt,
  chooseItem,
  chooseItem',
  underline,
  toText
  )


instance ConsoleText Card where
  toText = show


instance ConsoleText PlayerId where
  toText = show


instance ConsoleText Round where

  toText round =
    "Cards remaining: " ++ (show $ remainingCards round) ++ ".\n\n" ++
    underline '-' "All discards" ++ "\n" ++
    Map.foldrWithKey (\k a b -> formatPlayer k a ++ "\n" ++ b) "" (getPlayerMap round)
    where
      formatPlayer pid player =
        toText pid ++ ": " ++ intercalate ", " (map toText (getDiscards player)) ++ playerStatus player
      playerStatus player =
        case isProtected player of
         Just True -> " (protected)"
         Just False -> ""
         Nothing -> " (eliminated)"


instance ConsoleText Event where

  -- FIXME: Don't have quite enough information here to disambiguate between
  -- Soldier attack failing due to wrong guess and Soldier attack failing due to
  -- Priestess.
  toText (Played (viewAction -> (pid1, Soldier, Guess pid2 card)) NoChange) =
    toText pid1 ++ " wrongly guessed " ++ toText pid2 ++ " had a " ++ toText card
    ++ ". Nothing happened, maybe it was the right guess and they were protected."

  -- FIXME: Don't have quite enough information here to disambiguate between
  -- Knight attack failing due to tie and Knight attack failing due to
  -- Priestess.
  toText (Played (viewAction -> (pid1, Knight, Attack pid2)) NoChange) =
    toText pid1 ++ " attacked " ++ toText pid2 ++ " with a Knight, but nothing happened. "
    ++ "Because of a bug in the software, you don't know if it's because of a tie or "
    ++ "because " ++ toText pid2 ++ " is protected by the Priestess."

  toText (Played (viewAction -> (pid1, card, Attack pid2)) NoChange) =
    toText pid1 ++ " played " ++ toText card ++ " against " ++ toText pid2 ++
    ", but they were protected by the Priestess, so nothing happened"

  toText (Played (viewAction -> (pid1, card, NoEffect)) NoChange) =
    toText pid1 ++ " played " ++ toText card

  toText (BustedOut pid c1 c2) =
    toText pid ++ " busted out, holding " ++ toText c1 ++ " and " ++ toText c2

  toText (Played (viewAction -> (pid, Priestess, _)) (Protected p))
    | p == pid =
        toText pid ++ " played Priestess, protecting themselves from harm"
    | otherwise = error "BUG: " ++ show pid ++ " played Priestess but ended up protecting " ++ show p

  toText (Played _ (SwappedHands pid1 pid2)) =
    toText pid2 ++ " swapped hands with " ++ toText pid1

  toText (Played (viewAction -> (pid1, Soldier, Guess pid2 card)) (Eliminated loser))
    | loser == pid2 =
        toText pid1 ++ " correctly guessed " ++ toText pid2 ++ " had a " ++ toText card
        ++ ". " ++ toText pid2 ++ " has been eliminated"
    | otherwise = error "BUG: Soldier attacked " ++ show pid2 ++ " but eliminated " ++ show loser

  toText (Played (viewAction -> (pid1, Knight, Attack pid2)) (Eliminated loser)) =
    toText pid1 ++ " attacked " ++ toText pid2 ++ " with a Knight " ++
    (if loser == pid1
     then "and lost. "
     else (if loser == pid2 then "and won. " else error "BUG: Knight!")) ++
    toText loser ++ " has been eliminated"

  toText (Played (viewAction -> (pid1, Prince, NoEffect)) (Eliminated loser))
    | loser == pid1 = toText pid1 ++ " played the Prince, eliminating themselves"
    | otherwise = error "BUG: " ++ show pid1 ++ " played Prince, but " ++ show loser ++ " eliminated."

  -- FIXME: There are two reasons they could lose here: 1. Discard Prince, 2.
  -- Discard last card. Disambiguate between them.
  toText (Played (viewAction -> (pid1, Wizard, Attack pid2)) (Eliminated loser))
    | loser == pid2 =
        toText pid1 ++ " played Wizard on " ++ toText pid2 ++ " forcing them to discard "
        ++ "and thus be eliminated from the round"
    | otherwise = error "BUG: Wizard attacked " ++ show pid2 ++ " but eliminated " ++ show loser

  toText (Played _ (ForcedDiscard pid)) =
    toText pid ++ " was forced to discard their hand and draw another card"

  -- XXX: This is revealed to all who are watching the console.
  toText (Played _ (ForcedReveal pid1 pid2 card)) =
    toText pid1 ++ ": " ++ toText pid2 ++ " is holding a " ++ toText card

  toText event = "UNKNOWN: " ++ show event


instance ConsoleText Victory where
  toText (SoleSurvivor pid card) =
    toText pid ++ " wins as the only remaining player, holding " ++ toText card
  toText (HighestCard card (winner:[]) _) =
    toText winner ++ " wins holding " ++ toText card
  toText (HighestCard card winners _) =
    "Many winners holding " ++ toText card ++ ": " ++ (intercalate ", " (map toText winners))


formatScores :: [(PlayerId, Int)] -> String
formatScores scores =
  underline '-' "Scores" ++ "\n" ++
  unlines (map formatScore scores)
  where formatScore (pid, score) = toText pid ++ ": " ++ toText score


pickNumPlayers :: IO Int
pickNumPlayers =
  repeatedlyPrompt "Pick number of players: " parseNumPlayers
  where
    parseNumPlayers s =
      case readMaybe s of
       Nothing -> Left errMsg
       Just i -> if 2 <= i && i <= 4 then Right i else Left errMsg
    errMsg = "Please enter a number between 2 and 4"


pickCardToPlay :: (Card, Card) -> IO Card
pickCardToPlay (dealt, hand) =
  chooseItem "\nPlease choose a card: " [dealt, hand]


pickPlay :: Card -> PlayerSet -> IO Play
pickPlay card players =
  case card of
   Soldier -> pickGuess players
   Clown -> pickAttack players
   Knight -> pickAttack players
   Priestess -> return NoEffect
   Wizard -> pickAttack players
   General -> pickAttack players
   Minister -> return NoEffect
   Prince -> return NoEffect


-- XXX: Exclude self-targeting when it's not legal
pickTarget :: PlayerSet -> IO PlayerId
pickTarget ps = chooseItem "\nPlease choose a target: " (toPlayers ps)

pickAttack :: PlayerSet -> IO Play
pickAttack players = fmap Attack (pickTarget players)

pickGuess :: PlayerSet -> IO Play
pickGuess players = do
  target <- pickTarget players
  guess <- pickGuessCard
  return $ Guess target guess
  where pickGuessCard = chooseItem' "\nWhat card do they have?" 2 [Clown ..]
