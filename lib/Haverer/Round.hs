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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Haverer.Round (
  -- A Round of a game of Love Letter.
  Round
  , makeRound
  , playTurn
  , playTurn'

    -- The results of playTurn
  , BadAction
  , Result(..)
  , Event(..)

    -- Information about a Round
  , currentPlayer
  , currentTurn
  , getActivePlayers
  , getPlayer
  , getPlayerMap
  , getPlayers
  , getWinners
  , nextPlayer
  , remainingCards

    -- The outcome of a Round
  , Victory(..)
  , victory

    -- Properties used for testing that rely on unexposed fields.
  , prop_allCardsPresent
  , prop_burnCardsSame
  , prop_multipleActivePlayers
  , prop_ringIsActivePlayers
  ) where

import BasicPrelude hiding (round)

import Control.Error
import Control.Monad.Except
import Control.Lens hiding (chosen)

import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Haverer.Action (
  BadPlay,
  bustingHand,
  Play(..),
  Action,
  getTarget,
  playToAction,
  viewAction)
import Haverer.Deck (Card(..), Complete, Deck, deal, Incomplete, pop)
import qualified Haverer.Deck as Deck
import Haverer.Player (
  bust,
  discardAndDraw,
  eliminate,
  getDiscards,
  getHand,
  isProtected,
  makePlayer,
  playCard,
  Player,
  PlayerSet,
  protect,
  swapHands,
  toPlayers,
  unprotect
  )

import Haverer.Internal.Error (assertRight, terror)
import Haverer.Internal.Ring (Ring, advance1, currentItem, dropItem1, makeRing, nextItem)
import qualified Haverer.Internal.Ring as Ring


-- XXX: Consider popping this out so that it's the constructor of the Round.
data RoundState = NotStarted | Turn Card | Playing | Over deriving Show


data Round playerId = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring playerId,
  _players :: Map playerId Player,
  _roundState :: RoundState,
  _burn :: Card
} deriving Show


makeLenses ''Round


-- | Make a new round, given a complete Deck and a set of players.
makeRound :: (Ord playerId, Show playerId) => Deck Complete -> PlayerSet playerId -> Round playerId
makeRound deck playerSet =
  nextTurn $ case deal deck (length playerList) of
   (remainder, Just cards) ->
     case pop remainder of
      (_, Nothing) -> terror ("Not enough cards for burn: " ++ show deck)
      (stack', Just burn') -> Round {
        _stack = stack',
        _playOrder = fromJust (makeRing playerList),
        _players = Map.fromList $ zip playerList (map makePlayer cards),
        _roundState = NotStarted,
        _burn = burn'
        }
   _ -> terror ("Given a complete deck - " ++ show deck ++ "- that didn't have enough cards for players - " ++ show playerSet)
  where playerList = toPlayers playerSet


-- | The number of cards remaining in the deck.
remainingCards :: Round playerId -> Int
remainingCards = length . Deck.toList . view stack


-- | The IDs of all of the players.
getPlayers :: Round playerId -> [playerId]
getPlayers = Map.keys . view players


-- | The IDs of all of the active players.
getActivePlayers :: Round playerId -> [playerId]
getActivePlayers = Ring.toList . view playOrder


-- TODO: Rather than exporting Player, export functions that will return
-- active state, protected state and discard pile. Then move Player data type
-- and functions to somewhere hidden (Internal maybe?), because it really is
-- just as implementation detail of Round.


-- | A map of player IDs to players.
getPlayerMap :: Round playerId -> Map playerId Player
getPlayerMap = view players


-- | Get the player with the given ID. Nothing if there is no such player.
getPlayer :: Ord playerId => Round playerId -> playerId -> Maybe Player
getPlayer round pid = view (players . at pid) round


-- | Draw a card from the top of the Deck. Returns the card and a new Round.
drawCard :: Round playerId -> (Round playerId, Maybe Card)
drawCard r =
  let (stack', card) = pop (view stack r) in
  (set stack stack' r, card)


-- | The ID of the current player. If the Round is over or not started, this
-- will be Nothing.
currentPlayer :: Round playerId -> Maybe playerId
currentPlayer rnd =
  case view roundState rnd of
   Over -> Nothing
   NotStarted -> Nothing
   Turn _ -> Just $ (currentItem . view playOrder) rnd
   Playing -> Just $ (currentItem . view playOrder) rnd


currentTurn :: Ord playerId => Round playerId -> Maybe (playerId, (Card, Card))
currentTurn rnd = do
  pid <- currentPlayer rnd
  hand <- getHand =<< getPlayer rnd pid
  d <- dealt
  return (pid, (d, hand))
  where
    dealt = case view roundState rnd of
      Turn d -> Just d
      _ -> Nothing


nextPlayer :: Round playerId -> Maybe playerId
nextPlayer rnd =
  case view roundState rnd of
   Over -> Nothing
   NotStarted -> Just $ currentItem playOrder'
   Turn _ -> Just $ nextItem playOrder'
   Playing -> Just $ nextItem playOrder'
  where playOrder' = view playOrder rnd


-- XXX: Would using a State monad make any of this code better?


-- | Progress the Round to the next turn.
nextTurn :: (Show playerId, Ord playerId) => Round playerId -> Round playerId
nextTurn round@(Round { _roundState = Over }) = round
nextTurn round@(Round { _roundState = NotStarted }) = drawCard' round
nextTurn (Round { _roundState = Turn _ } ) =
  error "Cannot advance to next turn while waiting for play."
nextTurn round =
  let round' = drawCard' round in
  case nextPlayer round' of
   Nothing -> set roundState Over round
   Just pid ->
     case advance1 (view playOrder round) of
      Left _ -> set roundState Over round
      Right newPlayOrder ->
        let round'' = assertRight "Couldn't unprotect current player: "
                      (modifyActivePlayer round' pid unprotect) in
         set playOrder newPlayOrder round''


drawCard' :: Round playerId -> Round playerId
drawCard' round =
  case drawCard round of
   (round', Just card) -> set roundState (Turn card) round'
   _ -> set roundState Over round


data BadAction playerId = NoSuchPlayer playerId
                        | InactivePlayer playerId
                        | InvalidPlay (BadPlay playerId)
                        | WrongCard Card (Card, Card)
                        | PlayWhenBusted
                        | NoPlaySpecified
                        | RoundOver
                        deriving Show


-- | A change to the Round that comes as result of a player's actions.
data Event playerId =
  -- | Nothing happened. What the player did had no effect.
  NoChange |
  -- | The player is now protected.
  Protected playerId |
  -- | The first player has been forced to swap hands with the second.
  SwappedHands playerId playerId |
  -- | The player has been eliminated from the round.
  Eliminated playerId |
  -- | The player has been forced to discard their hand.
  ForcedDiscard playerId |
  -- | The second player has been forced to show their hand to the first.
  ForcedReveal playerId playerId Card
  deriving (Eq, Show)


-- | The result of a turn.
data Result playerId =
  -- | The player whose turn it was "busted out", they held the Minister and
  -- another high card, and thus didn't get to play.
  BustedOut playerId Card Card |
  -- | The player performed an Action resulting in Event.
  Played (Action playerId) (Event playerId)
  deriving (Eq, Show)


type ActionM id a = Either (BadAction id) a

-- | Translate a player action into a change to make to the round.
-- Will return errors if the action is for or against an inactive or
-- nonexistent player.
--
-- If the target player is protected, will return the identity result,
-- NoChange.
actionToEvent :: (Ord playerId, Show playerId) => Round playerId -> Action playerId -> ActionM playerId (Event playerId)
actionToEvent round action@(viewAction -> (pid, _, play)) = do
  (_, sourceHand) <- getActivePlayerHand round pid
  case getTarget play of
   Nothing -> return $ noTarget action
   Just target -> do
     (targetPlayer, targetHand) <- getActivePlayerHand round target
     if fromJust (isProtected targetPlayer)
       then return NoChange
       else return $ withTarget sourceHand targetHand action

  where

    noTarget (viewAction -> (_, Priestess, NoEffect)) = Protected pid
    noTarget (viewAction -> (_, Minister, NoEffect)) = NoChange
    noTarget (viewAction -> (_, Prince, NoEffect)) = Eliminated pid
    noTarget _ = terror $ "We thought " ++ show action ++ " had no target."

    withTarget _ targetCard (viewAction -> (_, Soldier, Guess target guess))
      | targetCard == guess = Eliminated target
      | otherwise = NoChange
    withTarget _ targetCard (viewAction -> (_, Clown, Attack target)) =
      ForcedReveal pid target targetCard
    withTarget sourceHand targetHand (viewAction -> (_, Knight, Attack target)) =
      case compare sourceHand targetHand of
       LT -> Eliminated pid
       EQ -> NoChange
       GT -> Eliminated target
    withTarget _ Prince (viewAction -> (_, Wizard, Attack target)) = Eliminated target
    withTarget _ _ (viewAction -> (_, Wizard, Attack target)) = ForcedDiscard target
    withTarget _ _ (viewAction -> (_, General, Attack target)) = SwappedHands target pid
    withTarget _ _ _ = terror $ "Invalid action: " ++ show action


-- XXX: Lots of these re-get players from the Round that have already been
-- retrieved by actionToEvent. Perhaps we could include that data in the Event
-- structure so this simply returns a Round.

-- | Apply a change to the Round.
applyEvent :: Ord playerId => Round playerId -> Event playerId -> ActionM playerId (Round playerId)
applyEvent round NoChange = return round
applyEvent round (Protected pid) = modifyActivePlayer round pid protect
applyEvent round (SwappedHands pid1 pid2) = do
  p1 <- getActivePlayer round pid1
  p2 <- getActivePlayer round pid2
  let (p1', p2') = swapHands p1 p2 in
   return $ (replace pid2 p2' . replace pid1 p1') round
   where replace pid p rnd = setActivePlayer rnd pid p
applyEvent round (Eliminated pid) = modifyActivePlayer round pid eliminate
applyEvent round (ForcedDiscard pid) =
  let (round', card) = drawCard round in
  modifyActivePlayer round' pid (`discardAndDraw` card)
applyEvent round (ForcedReveal {}) = return round


-- | Play a turn in a Round.
--
-- This is the main function in this module.
--
-- A turn has two steps. First, the player draws a card. If their hand "busts
-- out" (due to holding the Minister and another high card), then they are
-- eliminated and play proceeds to the next player. This is the `Left` return
-- value, which returns the new Round and a Result indicating the player bust
-- out.
--
-- Second, the player plays one of these two cards. This is the `Right` return
-- value, a function that takes the players chosen card and play, and returns
-- either a BadAction or a new Round together with the Result of the play.
playTurn :: (Ord playerId, Show playerId)
            => Round playerId
            -> Either (ActionM playerId (Round playerId, Result playerId))
                      (Card -> Play playerId -> ActionM playerId (Round playerId, Result playerId))
playTurn round = do
  (playerId, (dealt, hand)) <- note (Left RoundOver) (currentTurn round)
  let player = assertRight "Current player is not active: " (getActivePlayer round playerId)
  if bustingHand dealt hand
    then Left $ return $ bustOut playerId dealt hand
    else Right $ handlePlay playerId player dealt hand

  where
    handlePlay playerId player dealt hand chosen play = do
      player' <- note (WrongCard chosen (dealt, hand)) (playCard player dealt chosen)
      let round' = setActivePlayer (set roundState Playing round) playerId player'
      action <- fmapL InvalidPlay (playToAction playerId chosen play)
      result <- actionToEvent round' action
      round'' <- applyEvent round' result
      return (nextTurn round'', Played action result)

    bustOut pid dealt hand =
      let bustedRound = assertRight "Could not bust out player: "
                                    (modifyActivePlayer round pid (`bust` dealt))
      in (nextTurn (set roundState Playing bustedRound), BustedOut pid dealt hand)


-- | Play a turn in a Round
--
-- Similar to playTurn, except that instead of splitting the turn into two
-- phases, there is a single, optional play. If the hand is a busting hand,
-- then the play must be Nothing; if not, the play must be specified.
playTurn' :: (Ord playerId, Show playerId)
             => Round playerId
             -> Maybe (Card, Play playerId)
             -> ActionM playerId (Round playerId, Result playerId)
playTurn' round optionalPlay = do
  result <- case playTurn round of
             Left action -> action
             Right handler -> do
               (card, play) <- note NoPlaySpecified optionalPlay
               handler card play
  case (optionalPlay, result) of
   (Just _, (_, BustedOut {})) -> throwError PlayWhenBusted
   _ -> return result


data Victory playerId
  -- | The given player is the only survivor.
  = SoleSurvivor playerId Card
  -- | These players have the highest card.
  | HighestCard Card [playerId] [(playerId, Card)]
  deriving (Eq, Show)


-- | The currently surviving players in the round, with their cards.
survivors :: Round playerId -> [(playerId, Card)]
survivors = Map.toList . Map.mapMaybe getHand . view players


-- | If the Round is Over, return the Victory data. Otherwise, Nothing.
victory :: Round playerId -> Maybe (Victory playerId)
victory (round@Round { _roundState = Over }) =
  case survivors round of
   [(pid, card)] -> Just $ SoleSurvivor pid card
   xs -> let (best:rest) = reverse (groupBy ((==) `on` snd) (sortBy (compare `on` snd) xs))
         in Just $ HighestCard (snd $ head best) (map fst best) (concat rest)
victory _ = Nothing


getWinners :: Victory playerId -> [playerId]
getWinners (SoleSurvivor pid _) = [pid]
getWinners (HighestCard _ pids _) = pids


-- | Update the given player in Round. If the update function returns Nothing,
-- then that is taken to mean the player was inactive.
modifyActivePlayer :: Ord playerId => Round playerId -> playerId -> (Player -> Player) -> ActionM playerId (Round playerId)
modifyActivePlayer rnd pid f = setActivePlayer rnd pid <$> f <$> getActivePlayer rnd pid


-- | Replace the given player in the Round. If the new player is inactive,
-- then the player is dropped from the cycle of play.
setActivePlayer :: Ord playerId => Round playerId -> playerId -> Player -> Round playerId
setActivePlayer round pid newP =
  case getHand newP of
   Nothing -> dropPlayer pid
   Just _ -> round'
  where
    round' = over players (set (at pid) (Just newP)) round
    dropPlayer p =
      case dropItem1 (view playOrder round') p of
       Left _ -> set roundState Over round
       Right newOrder -> set playOrder newOrder round'


-- | Get the given player, asserting they must be active. Will return a Left
-- if no player is found or if the requested player is inactive.
getActivePlayer :: Ord playerId => Round playerId -> playerId -> ActionM playerId Player
getActivePlayer round pid = fst <$> getActivePlayerHand round pid


getActivePlayerHand :: Ord playerId => Round playerId -> playerId -> ActionM playerId (Player, Card)
getActivePlayerHand round pid = do
  player <- note (NoSuchPlayer pid) (getPlayer round pid)
  hand <- note (InactivePlayer pid) (getHand player)
  return (player, hand)


-- | Are all the cards in the Round?
prop_allCardsPresent :: Round playerId -> Bool
prop_allCardsPresent =
  isJust . Deck.makeDeck . allCards
  where allCards rnd =
          _burn rnd : (
            (Deck.toList . view stack) rnd
            ++ (concatMap getDiscards . Map.elems . view players) rnd
            ++ (mapMaybe getHand . Map.elems . view players) rnd)
            ++ (
            case _roundState rnd of
              Turn x -> [x]
              _ -> [])


prop_burnCardsSame :: [Round playerId] -> Bool
prop_burnCardsSame (x:xs) = all ((== view burn x) . view burn) xs
prop_burnCardsSame [] = True


prop_ringIsActivePlayers :: Eq playerId => Round playerId -> Bool
prop_ringIsActivePlayers r =
  (Map.keys . Map.mapMaybe getHand . _players) r ==
  (Ring.toList . view playOrder) r


prop_multipleActivePlayers :: Round playerId -> Bool
prop_multipleActivePlayers r =
  case view roundState r of
   Over -> True
   _ -> Ring.ringSize (view playOrder r) > 1
