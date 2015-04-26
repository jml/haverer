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
{-# LANGUAGE ViewPatterns #-}

module Haverer.Round (
  -- A Round of a game of Love Letter.
  Round
  , makeRound
  , playTurn

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

import Prelude hiding (round)

import Control.Applicative ((<$>))
import Control.Lens hiding (chosen)

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, isJust, maybeToList)
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

import Haverer.Internal.Ring (Ring, advance1, currentItem, dropItem1, makeRing, nextItem)
import qualified Haverer.Internal.Ring as Ring


-- XXX: Consider popping this out so that it's the constructor of the Round.
data State = NotStarted | Turn Card | Playing | Over deriving Show


data Round playerId = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring playerId,
  _players :: Map.Map playerId Player,
  _state :: State,
  _burn :: Card
} deriving Show


makeLenses ''Round


-- | Make a new round, given a complete Deck and a set of players.
makeRound :: (Ord playerId, Show playerId) => Deck Complete -> PlayerSet playerId -> Round playerId
makeRound deck playerSet =
  nextTurn $ case deal deck (length playerList) of
   (remainder, Just cards) ->
     case pop remainder of
      (_, Nothing) -> error ("Not enough cards for burn: " ++ show deck)
      (stack', Just burn') -> Round {
        _stack = stack',
        _playOrder = fromJust (makeRing playerList),
        _players = Map.fromList $ zip playerList (map makePlayer cards),
        _state = NotStarted,
        _burn = burn'
        }
   _ -> error ("Given a complete deck - " ++ show deck ++ "- that didn't have enough cards for players - " ++ show playerSet)
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
getPlayerMap :: Round playerId -> Map.Map playerId Player
getPlayerMap = view players


-- | Get the player with the given ID. Nothing if there is no such player.
getPlayer :: Ord playerId => Round playerId -> playerId -> Maybe Player
getPlayer round pid = (view (players . at pid) round)


-- | Draw a card from the top of the Deck. Returns the card and a new Round.
drawCard :: Round playerId -> (Round playerId, Maybe Card)
drawCard r =
  let (stack', card) = pop (view stack r) in
  (set stack stack' r, card)


-- | The ID of the current player. If the Round is over or not started, this
-- will be Nothing.
currentPlayer :: Round playerId -> Maybe playerId
currentPlayer rnd =
  case view state rnd of
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
    dealt = case view state rnd of
      Turn d -> Just d
      _ -> Nothing


nextPlayer :: Round playerId -> Maybe playerId
nextPlayer rnd =
  case view state rnd of
   Over -> Nothing
   NotStarted -> Just $ (currentItem playOrder')
   Turn _ -> Just $ nextItem playOrder'
   Playing -> Just $ nextItem playOrder'
  where playOrder' = view playOrder rnd


-- XXX: Would using a State monad make any of this code better?


-- | Progress the Round to the next turn.
nextTurn :: (Show playerId, Ord playerId) => Round playerId -> Round playerId
nextTurn round@(Round { _state = Over }) = round
nextTurn round@(Round { _state = NotStarted }) = drawCard' round
nextTurn (Round { _state = Turn _ } ) =
  error "Cannot advance to next turn while waiting for play."
nextTurn round =
  let round' = drawCard' round in
  case nextPlayer round' of
   Nothing -> set state Over round
   Just pid ->
     case advance1 (view playOrder round) of
      Left _ -> set state Over round
      Right newPlayOrder ->
        case modifyActivePlayer round' pid unprotect of
         Left e -> error $ "Couldn't unprotect current player: " ++ (show e)
         Right round'' -> set playOrder newPlayOrder round''


drawCard' :: Round playerId -> Round playerId
drawCard' round =
  case drawCard round of
   (round', Just card) -> set state (Turn card) round'
   _ -> set state Over round


data BadAction playerId = NoSuchPlayer playerId
                        | InactivePlayer playerId
                        | InvalidPlay (BadPlay playerId)
                        | WrongCard Card (Card, Card)
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
  Played (Action playerId) (Event playerId) |
  -- | The round was already over.
  RoundOver
  deriving (Eq, Show)


-- | Translate a player action into a change to make to the round.
-- Will return errors if the action is for or against an inactive or
-- nonexistent player.
--
-- If the target player is protected, will return the identity result,
-- NoChange.
actionToEvent :: (Ord playerId, Show playerId) => Round playerId -> Action playerId -> Either (BadAction playerId) (Event playerId)
actionToEvent round action@(viewAction -> (pid, _, play)) = do
  (_, sourceHand) <- getActivePlayerHand round pid
  case getTarget play of
   Nothing -> return $ actionToEvent' sourceHand Nothing action
   Just target -> do
     (targetPlayer, targetHand) <- getActivePlayerHand round target
     if fromJust (isProtected targetPlayer)
       then return NoChange
       else return $ actionToEvent' sourceHand (Just targetHand) action


-- | Given the hand of the current player, the hand of the target (if there is
-- one), and the action being played, return the change we need to make.
actionToEvent' :: Show playerId => Card -> Maybe Card -> Action playerId -> Event playerId
actionToEvent' _ hand (viewAction -> (_, Soldier, Guess target guess)) =
  if fromJust hand == guess
  then Eliminated target
  else NoChange
actionToEvent' _ (Just targetCard) (viewAction -> (pid, Clown, Attack target)) =
  ForcedReveal pid target targetCard
actionToEvent' sourceHand targetHand (viewAction -> (pid, Knight, Attack target)) =
  case compare sourceHand (fromJust targetHand) of
    LT -> Eliminated pid
    EQ -> NoChange
    GT -> Eliminated target
actionToEvent' _ _ (viewAction -> (pid, Priestess, NoEffect)) = Protected pid
actionToEvent' _ hand (viewAction -> (_, Wizard, Attack target)) =
  case hand of
    Just Prince -> Eliminated target
    _ -> ForcedDiscard target
actionToEvent' _ _ (viewAction -> (pid, General, Attack target)) = SwappedHands target pid
actionToEvent' _ _ (viewAction -> (_, Minister, NoEffect)) = NoChange
actionToEvent' _ _ (viewAction -> (pid, Prince, NoEffect)) = Eliminated pid
actionToEvent' _ _ action = error $ "Invalid action: " ++ (show action)


-- XXX: Lots of these re-get players from the Round that have already been
-- retrieved by actionToEvent. Perhaps we could include that data in the Event
-- structure so this simply returns a Round.

-- | Apply a change to the Round.
applyEvent :: Ord playerId => Round playerId -> Event playerId -> Either (BadAction playerId) (Round playerId)
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
  modifyActivePlayer round' pid (flip discardAndDraw card)
applyEvent round (ForcedReveal _ _ _) = return round


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a


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
            -> Either (Round playerId, Result playerId)
                      (Card -> Play playerId
                       -> Either (BadAction playerId)
                                 (Round playerId, Result playerId))
playTurn round = do
  (playerId, (dealt, hand)) <- case (currentTurn round) of
                                Nothing -> Left $ (round, RoundOver)
                                Just r -> return r
  player <- case getActivePlayer round playerId of
             Left e -> error $ "Current player is not active: " ++ (show e)
             Right r -> return r
  -- The card the player chose is now put in front of them, and the card they
  -- didn't chose is now their hand.
  if bustingHand dealt hand
    then Left $ bustOut playerId dealt hand
    else Right $ handlePlay playerId player dealt hand

  where
    handlePlay playerId player dealt hand chosen play = do
      -- An Action is a valid player, card, play combination.
      player' <- maybeToEither (WrongCard chosen (dealt, hand)) (playCard player dealt chosen)
      let round' = setActivePlayer (set state Playing round) playerId player'
      action <- case playToAction playerId chosen play of
                 Left e -> Left $ InvalidPlay e
                 Right a -> return a
      result <- actionToEvent round' action
      round'' <- applyEvent round' result
      return $ (nextTurn round'', Played action result)

    bustOut pid dealt hand =
      case modifyActivePlayer round pid (flip bust dealt) of
       Left e -> error $ "Could not bust out player: " ++ (show e)
       Right round' -> (nextTurn (set state Playing round'), BustedOut pid dealt hand)


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
victory (round@Round { _state = Over }) =
  case survivors round of
   (pid, card):[] -> Just $ SoleSurvivor pid card
   xs -> let (best:rest) = reverse (groupBy ((==) `on` snd) (sortBy (compare `on` snd) xs))
         in Just $ HighestCard (snd $ head best) (map fst best) (concat rest)
victory _ = Nothing


getWinners :: Victory playerId -> [playerId]
getWinners (SoleSurvivor pid _) = [pid]
getWinners (HighestCard _ pids _) = pids


-- | Update the given player in Round. If the update function returns Nothing,
-- then that is taken to mean the player was inactive.
modifyActivePlayer :: Ord playerId => Round playerId -> playerId -> (Player -> Player) -> Either (BadAction playerId) (Round playerId)
modifyActivePlayer rnd pid f = setActivePlayer rnd pid <$> f <$> getActivePlayer rnd pid


-- | Replace the given player in the Round. If the new player is inactive,
-- then the player is dropped from the cycle of play.
setActivePlayer :: Ord playerId => Round playerId -> playerId -> Player -> Round playerId
setActivePlayer round pid newP =
  case getHand newP of
   Nothing -> dropPlayer pid
   Just _ -> round'
  where
    round' = over players (\x -> set (at pid) (Just newP) x) round
    dropPlayer p =
      case dropItem1 (view playOrder round') p of
       Left _ -> set state Over round
       Right newOrder -> set playOrder newOrder round'


-- | Get the given player, asserting they must be active. Will return a Left
-- if no player is found or if the requested player is inactive.
getActivePlayer :: Ord playerId => Round playerId -> playerId -> Either (BadAction playerId) Player
getActivePlayer round pid = fst <$> getActivePlayerHand round pid


getActivePlayerHand :: Ord playerId => Round playerId -> playerId -> Either (BadAction playerId) (Player, Card)
getActivePlayerHand round pid =
  case getPlayer round pid of
   Nothing -> Left $ NoSuchPlayer pid
   Just player ->
     case getHand player of
      Nothing -> Left $ InactivePlayer pid
      Just hand -> Right (player, hand)


-- | Are all the cards in the Round?
prop_allCardsPresent :: Round playerId -> Bool
prop_allCardsPresent =
  isJust . Deck.makeDeck . allCards
  where allCards rnd =
          _burn rnd : (
            (Deck.toList . view stack) rnd
            ++ (concatMap getDiscards . Map.elems . view players) rnd
            ++ (concatMap (maybeToList . getHand) . Map.elems . view players) rnd)
            ++ (
            case _state rnd of
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
  case view state r of
   Over -> True
   _ -> (Ring.ringSize $ view playOrder r) > 1
