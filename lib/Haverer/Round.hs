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

module Haverer.Round ( BadAction
                     , currentPlayer
                     , currentTurn
                     , Event(..)
                     , Result(..)
                     , getActivePlayers
                     , getPlayer
                     , getPlayerMap
                     , getPlayers
                     , getWinners
                     , makeRound
                     , nextPlayer
                     , playTurn
                     , remainingCards
                     , Round
                     , Victory(..)
                     , victory
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
  PlayerId,
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


data Round = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring PlayerId,
  _players :: Map.Map PlayerId Player,
  _state :: State,
  _burn :: Card
} deriving Show


makeLenses ''Round



makeRound :: Deck Complete -> PlayerSet -> Round
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


remainingCards :: Round -> Int
remainingCards = length . Deck.toList . view stack


getPlayers :: Round -> [PlayerId]
getPlayers = Map.keys . view players


getPlayerMap :: Round -> Map.Map PlayerId Player
getPlayerMap = view players


getActivePlayers :: Round -> [PlayerId]
getActivePlayers = Ring.toList . view playOrder


getPlayer :: Round -> PlayerId -> Maybe Player
getPlayer round pid = (view (players . at pid) round)


drawCard :: Round -> (Round, Maybe Card)
drawCard r =
  let (stack', card) = pop (view stack r) in
  (set stack stack' r, card)


currentPlayer :: Round -> Maybe PlayerId
currentPlayer rnd =
  case view state rnd of
   Over -> Nothing
   NotStarted -> Nothing
   Turn _ -> Just $ (currentItem . view playOrder) rnd
   Playing -> Just $ (currentItem . view playOrder) rnd


currentTurn :: Round -> Maybe (PlayerId, (Card, Card))
currentTurn rnd = do
  pid <- currentPlayer rnd
  hand <- getHand =<< getPlayer rnd pid
  d <- dealt
  return (pid, (d, hand))
  where
    dealt = case view state rnd of
      Turn d -> Just d
      _ -> Nothing


nextPlayer :: Round -> Maybe PlayerId
nextPlayer rnd =
  case view state rnd of
   Over -> Nothing
   NotStarted -> Just $ (currentItem playOrder')
   Turn _ -> Just $ nextItem playOrder'
   Playing -> Just $ nextItem playOrder'
  where playOrder' = view playOrder rnd


-- XXX: Would using a State monad make any of this code better?


nextTurn :: Round -> Round
nextTurn round@(Round { _state = Over }) = round
nextTurn round@(Round { _state = NotStarted }) = drawCard' round
nextTurn (Round { _state = Turn _ } ) =
  error "Cannot advance to next turn while waiting for play."
nextTurn round =
  let round' = drawCard' round in
  case nextPlayer round' of
   Just pid ->
     case advance1 (view playOrder round) of
      Left _ -> set state Over round
      Right newPlayOrder ->
        case getPlayer round pid >>= unprotect of
         Nothing -> error $ "Couldn't get current player as active: " ++ (show pid)
         Just player ->
           setActivePlayer (round' & playOrder .~ newPlayOrder) pid player
   _ -> set state Over round


drawCard' :: Round -> Round
drawCard' round =
  case drawCard round of
   (round', Just card) -> set state (Turn card) round'
   _ -> set state Over round


data BadAction = NoSuchPlayer PlayerId
               | InactivePlayer PlayerId
               | InvalidPlay BadPlay
               | WrongCard Card (Card, Card)
               deriving Show


-- | A change to the Round that comes as result of a player's actions.
data Event =
  -- | Nothing happened. What the player did had no effect.
  NoChange |
  -- | The player is now protected.
  Protected PlayerId |
  -- | The first player has been forced to swap hands with the second.
  SwappedHands PlayerId PlayerId |
  -- | The player has been eliminated from the round.
  Eliminated PlayerId |
  -- | The player has been forced to discard their hand.
  ForcedDiscard PlayerId |
  -- | The second player has been forced to show their hand to the first.
  ForcedReveal PlayerId PlayerId Card
  deriving (Eq, Show)


-- | The result of a turn.
data Result =
  -- | The player whose turn it was "busted out", they held the Minister and
  -- another high card, and thus didn't get to play.
  BustedOut PlayerId Card Card |
  -- | The player performed an Action resulting in Event.
  Played Action Event |
  -- | The round was already over.
  RoundOver
  deriving (Eq, Show)


-- XXX: applyAction: Not actually "applying" the action, more just figuring
-- out what the result of performing the play would be. Rename.

-- | Given the hand of the current player, the hand of the target (if there is
-- one), and the action being played, return the change we need to make.
applyAction' :: Card -> Maybe Card -> Action -> Event
applyAction' _ hand (viewAction -> (_, Soldier, Guess target guess)) =
  if fromJust hand == guess
  then Eliminated target
  else NoChange
applyAction' _ (Just targetCard) (viewAction -> (pid, Clown, Attack target)) =
  ForcedReveal pid target targetCard
applyAction' sourceHand targetHand (viewAction -> (pid, Knight, Attack target)) =
  case compare sourceHand (fromJust targetHand) of
    LT -> Eliminated pid
    EQ -> NoChange
    GT -> Eliminated target
applyAction' _ _ (viewAction -> (pid, Priestess, NoEffect)) = Protected pid
applyAction' _ hand (viewAction -> (_, Wizard, Attack target)) =
  case hand of
    Just Prince -> Eliminated target
    _ -> ForcedDiscard target
applyAction' _ _ (viewAction -> (pid, General, Attack target)) = SwappedHands target pid
applyAction' _ _ (viewAction -> (_, Minister, NoEffect)) = NoChange
applyAction' _ _ (viewAction -> (pid, Prince, NoEffect)) = Eliminated pid
applyAction' _ _ action = error $ "Invalid action: " ++ (show action)


-- | Translate a player action into a change to make to the round.
-- Will return errors if the action is for or against an inactive or
-- nonexistent player.
--
-- If the target player is protected, will return the identity result,
-- NoChange.
applyAction :: Round -> Action -> Either BadAction Event
applyAction round action@(viewAction -> (pid, _, play)) = do
  (_, sourceHand) <- getActivePlayerHand round pid
  case getTarget play of
   Nothing -> return $ applyAction' sourceHand Nothing action
   Just target -> do
     (targetPlayer, targetHand) <- getActivePlayerHand round target
     if fromJust (isProtected targetPlayer)
       then return NoChange
       else return $ applyAction' sourceHand (Just targetHand) action


-- XXX: Lots of these re-get players from the Round that have already been
-- retrieved by applyAction. Perhaps we could include that data in the Event
-- structure so this simply returns a Round.
applyEvent :: Round -> Event -> Either BadAction Round
applyEvent round NoChange = return round
applyEvent round (Protected pid) = modifyActivePlayer round pid protect
applyEvent round (SwappedHands pid1 pid2) = do
  p1 <- getActivePlayer round pid1
  p2 <- getActivePlayer round pid2
  case swapHands p1 p2 of
   Nothing -> error $ "Inconsistency! Players inactive when swapping hands."
   Just (p1', p2') -> return $ (replace pid2 p2' . replace pid1 p1') round
   where replace pid p rnd = setActivePlayer rnd pid p
applyEvent round (Eliminated pid) = modifyActivePlayer round pid eliminate
applyEvent round (ForcedDiscard pid) =
  let (round', card) = drawCard round in
  do
    player <- getActivePlayer round pid
    case discardAndDraw player card of
     Nothing -> Left $ InactivePlayer pid  -- XXX: Really shouldn't happen.
     Just player'
       | player' == player -> return round
       | otherwise -> return $ setActivePlayer round' pid player'
applyEvent round (ForcedReveal _ _ _) = return round



maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a


playTurn :: Round -> Either (Round, Result) (Card -> Play -> Either BadAction (Round, Result))
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
      result <- applyAction round' action
      round'' <- applyEvent round' result
      return $ (nextTurn round'', Played action result)

    bustOut pid dealt hand =
      case modifyActivePlayer round pid (flip bust dealt) of
       Left e -> error $ "Could not bust out player: " ++ (show e)
       Right round' -> (nextTurn (set state Playing round'), BustedOut pid dealt hand)


data Victory
  -- | The given player is the only survivor.
  = SoleSurvivor PlayerId Card
  -- | These players have the highest card.
  | HighestCard Card [PlayerId] [(PlayerId, Card)]
  deriving (Eq, Show)


-- | The currently surviving players in the round, with their cards.
survivors :: Round -> [(PlayerId, Card)]
survivors = Map.toList . Map.mapMaybe getHand . view players


-- | If the Round is Over, return the Victory data. Otherwise, Nothing.
victory :: Round -> Maybe Victory
victory (round@Round { _state = Over }) =
  case survivors round of
   (pid, card):[] -> Just $ SoleSurvivor pid card
   xs -> let (best:rest) = reverse (groupBy ((==) `on` snd) (sortBy (compare `on` snd) xs))
         in Just $ HighestCard (snd $ head best) (map fst best) (concat rest)
victory _ = Nothing


getWinners :: Victory -> [PlayerId]
getWinners (SoleSurvivor pid _) = [pid]
getWinners (HighestCard _ pids _) = pids


-- | Update the given player in Round. If the update function returns Nothing,
-- then that is taken to mean the player was inactive.
modifyActivePlayer :: Round -> PlayerId -> (Player -> Maybe Player) -> Either BadAction Round
modifyActivePlayer rnd pid f =
  case getPlayer rnd pid of
   Nothing -> Left $ NoSuchPlayer pid
   Just player ->
     case f player of
      Nothing -> Left $ InactivePlayer pid
      Just newP -> Right $ setActivePlayer rnd pid newP


-- | Replace the given player in the Round. If the new player is inactive,
-- then the player is dropped from the cycle of play.
setActivePlayer :: Round -> PlayerId -> Player -> Round
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
getActivePlayer :: Round -> PlayerId -> Either BadAction Player
getActivePlayer round pid = fst <$> getActivePlayerHand round pid


getActivePlayerHand :: Round -> PlayerId -> Either BadAction (Player, Card)
getActivePlayerHand round pid =
  case getPlayer round pid of
   Nothing -> Left $ NoSuchPlayer pid
   Just player ->
     case getHand player of
      Nothing -> Left $ InactivePlayer pid
      Just hand -> Right (player, hand)


-- | Are all the cards in the Round?
prop_allCardsPresent :: Round -> Bool
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


prop_burnCardsSame :: [Round] -> Bool
prop_burnCardsSame (x:xs) = all ((== view burn x) . view burn) xs
prop_burnCardsSame [] = True


prop_ringIsActivePlayers :: Round -> Bool
prop_ringIsActivePlayers r =
  (Map.keys . Map.mapMaybe getHand . _players) r ==
  (Ring.toList . view playOrder) r


prop_multipleActivePlayers :: Round -> Bool
prop_multipleActivePlayers r =
  case view state r of
   Over -> True
   _ -> (Ring.ringSize $ view playOrder r) > 1
