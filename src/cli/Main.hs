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


import Prelude hiding (round)

import Data.List (intercalate)
import Text.Read

import Haverer.Action (Play(..))
import Haverer.Deck (Card(..))
import qualified Haverer.Engine as E
import qualified Haverer.Game as Game
import Haverer.Player (PlayerId, PlayerSet, makePlayerSet, toPlayers)

import Haverer.CLI.CommandLine ()
import Haverer.CLI.Prompt (repeatedlyPrompt, chooseItem, chooseItem', underline, toText)


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

-- XXX: Exclude self-targeting when it's not legal

main :: IO ()
main = do
  result <- pickNumPlayers
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail $ "Couldn't make set for " ++ (show result) ++ " players"

  E.playGame players >> return ()


formatScores :: [(PlayerId, Int)] -> String
formatScores scores =
  underline '-' "Scores" ++ "\n" ++
  unlines (map formatScore scores)
  where formatScore (pid, score) = toText pid ++ ": " ++ toText score



instance E.MonadEngine IO where

  badPlay e = putStrLn $ "ERROR: " ++ (show e)

  choosePlay players _ dealt hand = do
      card <- pickCardToPlay (dealt, hand)
      putStrLn $ "You chose: " ++ show card
      play <- pickPlay card players
      return (card, play)

  handStarted = putStrLn . toText

  handOver event = do
     putStrLn $ "\n" ++ (toText event) ++ "\n"

  gameStarted _ = do
    putStrLn $ underline '=' "GAME BEGIN"
    putStrLn ""

  gameOver outcome = do
    putStrLn $ "GAME OVER"
    case Game.winners outcome of
     (x:[]) -> putStrLn $ "The winner is: " ++ toText x ++ "!"
     xs -> putStrLn $ "The winners are: " ++ intercalate ", " (map toText xs) ++ "!"
    putStrLn $ formatScores $ Game.finalScores outcome

  roundStarted game _ = do
    putStrLn $ formatScores $ Game.scores game
    putStrLn $ underline '=' ("ROUND #" ++ show (Game.roundsPlayed game + 1) ++ " BEGIN")
    putStrLn ""

  roundOver v = do
    putStrLn $ "ROUND OVER"
    putStrLn $ toText v
    putStrLn ""
