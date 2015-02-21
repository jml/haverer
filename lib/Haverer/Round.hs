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

module Haverer.Round ( BadAction
                     , currentPlayer
                     , currentTurn
                     , Event(..)  -- XXX: Probably shouldn't be exporting this
                     , Result(..)  -- XXX: Probably shouldn't be exporting this
                     , getActivePlayers
                     , getPlayer
                     , getPlayerMap
                     , getPlayers
                     , getWinners
                     , makeRound
                     , nextPlayer
                     , playTurn
                     , prop_allCardsPresent
                     , prop_burnCardsSame
                     , prop_multipleActivePlayers
                     , prop_ringIsActivePlayers
                     , remainingCards
                     , Round
                     , Victory(..)
                     , victory
                     ) where

import Prelude hiding (round)

import Control.Applicative ((<$>))
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


data Round = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring PlayerId,
  _players :: Map.Map PlayerId Player,
  _state :: State,
  _burn :: Card
} deriving Show


-- XXX: Consider popping this out so that it's the constructor of the Round.
data State = NotStarted | Turn Card | Playing | Over deriving Show

makeRound :: Deck Complete -> PlayerSet -> Round
makeRound deck players =
  nextTurn $ case deal deck (length playerList) of
   (remainder, Just cards) ->
     case pop remainder of
      (_, Nothing) -> error ("Not enough cards for burn: " ++ show deck)
      (stack, Just burn) -> Round {
        _stack = stack,
        _playOrder = fromJust (makeRing playerList),
        _players = Map.fromList $ zip playerList (map makePlayer cards),
        _state = NotStarted,
        _burn = burn
        }
   _ -> error ("Given a complete deck - " ++ show deck ++ "- that didn't have enough cards for players - " ++ show players)
  where playerList = toPlayers players


remainingCards :: Round -> Int
remainingCards = length . Deck.toList . _stack


getPlayers :: Round -> [PlayerId]
getPlayers = Map.keys . _players


getPlayerMap :: Round -> Map.Map PlayerId Player
getPlayerMap = _players


getActivePlayers :: Round -> [PlayerId]
getActivePlayers = Ring.toList . _playOrder


getPlayer :: Round -> PlayerId -> Maybe Player
getPlayer Round { _players = players } pid = Map.lookup pid players


getActivePlayer :: Round -> PlayerId -> Either BadAction Player
getActivePlayer round pid = fst <$> getActivePlayerHand round pid


getPlayerHand :: Round -> PlayerId -> Maybe Card
getPlayerHand r pid = getHand =<< getPlayer r pid


getActivePlayerHand :: Round -> PlayerId -> Either BadAction (Player, Card)
getActivePlayerHand round pid =
  case getPlayer round pid of
   Nothing -> Left $ NoSuchPlayer pid
   Just player ->
     case getHand player of
      Nothing -> Left $ InactivePlayer pid
      Just hand -> Right (player, hand)


drawCard :: Round -> (Round, Maybe Card)
drawCard r =
  let (stack, card) = pop (_stack r) in
  (r { _stack = stack }, card)


currentPlayer :: Round -> Maybe PlayerId
currentPlayer rnd =
  case _state rnd of
   Over -> Nothing
   NotStarted -> Nothing
   Turn _ -> Just $ (currentItem . _playOrder) rnd
   Playing -> Just $ (currentItem . _playOrder) rnd


currentHand :: Round -> Maybe (Card, Card)
currentHand rnd = do
  pid <- currentPlayer rnd
  hand <- getPlayerHand rnd pid
  case _state rnd of
   Turn dealt -> return (dealt, hand)
   _ -> Nothing


currentTurn :: Round -> Maybe (PlayerId, (Card, Card))
currentTurn rnd = do
  pid <- currentPlayer rnd
  cards <- currentHand rnd
  return (pid, cards)


nextPlayer :: Round -> Maybe PlayerId
nextPlayer rnd =
  case _state rnd of
   Over -> Nothing
   NotStarted -> Just $ (currentItem playOrder)
   Turn _ -> Just $ nextItem playOrder
   Playing -> Just $ nextItem playOrder
  where playOrder = _playOrder rnd


-- XXX: Would using a State monad make any of this code better?


nextTurn :: Round -> Round
nextTurn r@(Round { _state = Over }) = r
-- XXX: This is kind of crap. Either eliminate the duplication using some kind
-- of composable operation, or just get rid of the NotStarted state, since we
-- basically don't use it.
nextTurn r@(Round { _state = NotStarted }) =
  case drawCard r of
   (r2, Just card) -> r2 { _state = Turn card }
   _ -> error $ "Cannot draw a card in just started round: " ++ show r
nextTurn (Round { _state = Turn _ } ) =
  error "Cannot advance to next turn while waiting for play."
nextTurn r =
  case (drawCard r, nextPlayer r) of
   ((r2, Just card), Just pid) ->
     case advance1 (_playOrder r) of
      Left _ -> r { _state = Over }
      Right newPlayOrder ->
        case getPlayer r pid >>= unprotect of
         Nothing -> error $ "Couldn't get current player as active: " ++ (show pid)
         Just player ->
           replacePlayer (
             r2 { _state = Turn card,
                  _playOrder = newPlayOrder }) pid player
   _ -> r { _state = Over }



data BadAction = NoSuchPlayer PlayerId
               | InactivePlayer PlayerId
               | InvalidPlay BadPlay
               | WrongCard Card (Card, Card)
               deriving Show


-- XXX: Terrible name
data Result =
  NoChange |
  Protected PlayerId |
  SwappedHands PlayerId PlayerId |
  Eliminated PlayerId |
  ForcedDiscard PlayerId |
  ForcedReveal PlayerId PlayerId Card
  deriving (Eq, Show)


-- XXX: Terrible name
data Event =
  BustedOut PlayerId Card Card |
  Played Action Result |
  RoundOver
  deriving (Eq, Show)


-- XXX: applyAction: Not actually "applying" the action, more just figuring
-- out what the result of performing the play would be. Rename.

-- | Given the hand of the current player, the hand of the target (if there is
-- one), and the action being played, return the change we need to make.
applyAction' :: Card -> Maybe Card -> Action -> Result
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
applyAction :: Round -> Action -> Either BadAction Result
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
-- retrieved by applyAction. Perhaps we could include that data in the Result
-- structure so this simply returns a Round.
applyResult :: Round -> Result -> Either BadAction Round
applyResult round NoChange = return round
applyResult round (Protected pid) = adjustPlayer round pid protect
applyResult round (SwappedHands pid1 pid2) = do
  p1 <- getActivePlayer round pid1
  p2 <- getActivePlayer round pid2
  case swapHands p1 p2 of
   Nothing -> error $ "Inconsistency! Players inactive when swapping hands."
   Just (p1', p2') -> return $ (replace pid2 p2' . replace pid1 p1') round
   where replace pid p rnd = replacePlayer rnd pid p
applyResult round (Eliminated pid) = adjustPlayer round pid eliminate
applyResult round (ForcedDiscard pid) =
  let (round', card) = drawCard round in
  do
    player <- getActivePlayer round pid
    case discardAndDraw player card of
     Nothing -> Left $ InactivePlayer pid  -- XXX: Really shouldn't happen.
     Just player'
       | player' == player -> return round
       | otherwise -> return $ replacePlayer round' pid player'
applyResult round (ForcedReveal _ _ _) = return round



maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a


playTurn :: Round -> Either (Round, Event) (Card -> Play -> Either BadAction (Round, Event))
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
      let round' = replacePlayer (round { _state = Playing }) playerId player'
      action <- case playToAction playerId chosen play of
                 Left e -> Left $ InvalidPlay e
                 Right a -> return a
      result <- applyAction round' action
      round'' <- applyResult round' result
      return $ (nextTurn round'', Played action result)

    bustOut pid dealt hand =
      case adjustPlayer round pid (flip bust dealt) of
       Left e -> error $ "Could not bust out player: " ++ (show e)
       Right round' -> (nextTurn (round' { _state = Playing }), BustedOut pid dealt hand)


data Victory
  -- | The given player is the only survivor.
  = SoleSurvivor PlayerId Card
  -- | These players have the highest card.
  | HighestCard Card [PlayerId] [(PlayerId, Card)]
  deriving (Eq, Show)


-- | The currently surviving players in the round, with their cards.
survivors :: Round -> [(PlayerId, Card)]
survivors = Map.toList . Map.mapMaybe getHand . _players


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


adjustPlayer :: Round -> PlayerId -> (Player -> Maybe Player) -> Either BadAction Round
adjustPlayer rnd pid f =
  case getPlayer rnd pid of
   Nothing -> Left $ NoSuchPlayer pid
   Just player ->
     case f player of
      Nothing -> Left $ InactivePlayer pid
      Just newP -> Right $ replacePlayer rnd pid newP


replacePlayer :: Round -> PlayerId -> Player -> Round
replacePlayer rnd pid newP =
  case getHand newP of
   Nothing -> dropPlayer pid
   Just _ -> newRnd
  where
    newRnd = rnd { _players = Map.insert pid newP (_players rnd) }
    dropPlayer p =
      case dropItem1 (_playOrder newRnd) p of
       Left _ -> rnd { _state = Over }
       Right newOrder -> newRnd { _playOrder = newOrder }


-- | Are all the cards in the Round?
prop_allCardsPresent :: Round -> Bool
prop_allCardsPresent =
  isJust . Deck.makeDeck . allCards
  where allCards rnd =
          _burn rnd : (
            (Deck.toList . _stack) rnd
            ++ (concatMap getDiscards . Map.elems . _players) rnd
            ++ (concatMap (maybeToList . getHand) . Map.elems . _players) rnd)
            ++ (
            case _state rnd of
              Turn x -> [x]
              _ -> [])


prop_burnCardsSame :: [Round] -> Bool
prop_burnCardsSame (x:xs) = all ((== _burn x) . _burn) xs
prop_burnCardsSame [] = True


prop_ringIsActivePlayers :: Round -> Bool
prop_ringIsActivePlayers r =
  (Map.keys . Map.mapMaybe getHand . _players) r ==
  (Ring.toList . _playOrder) r


prop_multipleActivePlayers :: Round -> Bool
prop_multipleActivePlayers r =
  case _state r of
   Over -> True
   _ -> (Ring.ringSize . _playOrder $ r) > 1
