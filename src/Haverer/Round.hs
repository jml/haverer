{-# LANGUAGE ViewPatterns #-}

module Haverer.Round ( BadAction
                     , currentPlayer
                     , currentTurn
                     , getActivePlayers
                     , getPlayer
                     , getPlayers
                     , newRound
                     , nextPlayer
                     , playTurn
                     , prop_allCardsPresent
                     , prop_burnCardsSame
                     , prop_multipleActivePlayers
                     , prop_ringIsActivePlayers
                     , Round
                     ) where

import Prelude hiding (round)

import Control.Applicative ((<$>))
import Data.Maybe (fromJust, isJust, maybeToList)
import qualified Data.Map as Map

import Haverer.Action (BadPlay, Play(..), Action, playToAction, viewAction)
import Haverer.Deck (Card(..), Complete, Deck, deal, Incomplete, pop)
import qualified Haverer.Deck as Deck
import Haverer.Player (
  discardAndDraw,
  eliminate,
  getDiscards,
  getHand,
  newPlayer,
  playCard,
  PlayerId,
  Player,
  PlayerSet,
  protect,
  swapHands,
  toPlayers,
  unprotect
  )
import Haverer.Ring (Ring, advance1, currentItem, dropItem1, newRing, nextItem)
import qualified Haverer.Ring as Ring


data Round = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring PlayerId,
  _players :: Map.Map PlayerId Player,
  _state :: State,
  -- XXX: Don't really *need* the following, but they've been useful for debugging.
  _burn :: Card,
  _log :: [Event],
  _deck :: Deck Complete
} deriving Show


data State = NotStarted | Turn Card | Playing | Over deriving Show

newRound :: Deck Complete -> PlayerSet -> Round
newRound deck players =
  nextTurn $ case deal deck (length playerList) of
   (remainder, Just cards) ->
     case pop remainder of
      (_, Nothing) -> error ("Not enough cards for burn: " ++ show deck)
      (stack, Just burn) -> Round {
        _stack = stack,
        _playOrder = fromJust (newRing playerList),
        _players = Map.fromList $ zip playerList (map newPlayer cards),
        _state = NotStarted,
        _burn = burn,
        _log = [],
        _deck = deck
        }
   _ -> error ("Given a complete deck - " ++ show deck ++ "- that didn't have enough cards for players - " ++ show players)
  where playerList = toPlayers players


getPlayers :: Round -> [PlayerId]
getPlayers = Map.keys . _players


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
               | RoundOver
               deriving Show


data Result =
  NothingHappened |
  Protected PlayerId |
  SwappedHands PlayerId PlayerId |
  Eliminated PlayerId |
  ForcedDiscard PlayerId |
  ForcedReveal PlayerId PlayerId
  deriving (Eq, Show)


data Event =
  BustedOut PlayerId Card Card |
  Played Action Result
  deriving (Eq, Show)


-- XXX: An alternative approach is to wrap this in a function that does a lot
-- of the preliminary checks:
--   * is the action for the current player
--   * is the target (if present) active
--   * is the target (if active) protected
applyAction :: Round -> Action -> Either BadAction Result
applyAction round (viewAction -> (_, Soldier, Guess target guess)) = do
  (_, hand) <- getActivePlayerHand round target
  return $ if hand == guess then Eliminated target else NothingHappened
applyAction _ (viewAction -> (pid, Clown, Attack target)) =
  -- XXX: Not checking that target is active.
  return $ ForcedReveal pid target
applyAction round (viewAction -> (pid, Knight, Attack target)) = do
  (_, sourceHand) <- getActivePlayerHand round pid
  (_, targetHand) <- getActivePlayerHand round target
  return $ case compare sourceHand targetHand of
    LT -> Eliminated pid
    EQ -> NothingHappened
    GT -> Eliminated target
applyAction _ (viewAction -> (pid, Priestess, NoEffect)) =
  return $ Protected pid
applyAction round (viewAction -> (_, Wizard, Attack target)) = do
  (_, hand) <- getActivePlayerHand round target
  return $ case hand of
    Prince -> Eliminated target
    _ -> ForcedDiscard target
applyAction round (viewAction -> (pid, General, Attack target)) = do
  _ <- getActivePlayerHand round pid
  _ <- getActivePlayerHand round target
  -- XXX: Order matters here, easy to get wrong. Another reason for pushing
  -- protection checking up a level.
  return $ SwappedHands target pid
applyAction _ (viewAction -> (_, Minister, NoEffect)) =
  return $ NothingHappened
applyAction _ (viewAction -> (pid, Prince, NoEffect)) =
  return $ Eliminated pid
applyAction _ action = error $ "Invalid action: " ++ (show action)

-- XXX: Lots of these re-get players from the Round that have already been
-- retrieved by applyAction. Perhaps we could include that data in the Result
-- structure so this simply returns a Round.
applyResult :: Round -> Result -> Either BadAction Round
applyResult round NothingHappened = return round
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
applyResult round (ForcedReveal _ _) = return round


-- FIXME: No way to send Clown / ForceReveal results

-- FIXME: No way for communicating busting out due to minister


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a


playTurn :: Round -> Card -> Play -> Either BadAction (Round, Event)
playTurn round chosen play = do
  (playerId, (dealt, hand)) <- maybeToEither RoundOver (currentTurn round)
  player <- getActivePlayer round playerId
  player' <- maybeToEither (WrongCard chosen (dealt, hand)) (playCard player dealt chosen)
  let round' = replacePlayer (round { _state = Playing }) playerId player'
  action <- case playToAction playerId chosen play of
             Left e -> Left $ InvalidPlay e -- XXX: Bad play. Translate to error type.
             Right a -> return a
  result <- applyAction round' action
  round'' <- applyResult round' result
  return $ (nextTurn round'', Played action result)


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
