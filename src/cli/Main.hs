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

import qualified Haverer.Engine as E
import qualified Haverer.Game as Game
import Haverer.Player (makePlayerSet)

import Haverer.CLI.CommandLine (
  formatScores,
  pickNumPlayers,
  pickCardToPlay,
  pickPlay,
  )
import Haverer.CLI.Prompt (underline, toText)


main :: IO ()
main = do
  result <- pickNumPlayers
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail $ "Couldn't make set for " ++ (show result) ++ " players"

  E.playGame players >> return ()


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