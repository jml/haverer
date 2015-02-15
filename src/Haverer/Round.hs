{-# LANGUAGE ViewPatterns #-}

module Haverer.Round ( BadAction
                     , currentPlayer
                     , currentTurn
                     , Event(..)  -- XXX: Probably shouldn't be exporting this
                     , Result(..)  -- XXX: Probably shouldn't be exporting this
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
                     , victory
                     ) where

import Prelude hiding (round)

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy)
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
  discardAndDraw,
  eliminate,
  getDiscards,
  getHand,
  isProtected,
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
import Haverer.Prompt (ConsoleText, toText, underline)
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



instance ConsoleText Round where

  toText round =
    "Cards remaining: " ++ (show . length . Deck.toList . _stack $ round) ++ ".\n\n" ++
    underline '-' "All discards" ++ "\n" ++
    Map.foldrWithKey (\k a b -> formatPlayer k a ++ "\n" ++ b) "" (_players round)
    where
      formatPlayer pid player =
        toText pid ++ ": " ++ intercalate ", " (map toText (getDiscards player)) ++ playerStatus player
      playerStatus player =
        case isProtected player of
         Just True -> " (protected)"
         Just False -> ""
         Nothing -> " (eliminated)"


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
  ForcedReveal PlayerId PlayerId Card
  deriving (Eq, Show)


data Event =
  BustedOut PlayerId Card Card |
  Played Action Result
  deriving (Eq, Show)


instance ConsoleText Event where

  -- FIXME: Don't have quite enough information here to disambiguate between
  -- Soldier attack failing due to wrong guess and Soldier attack failing due to
  -- Priestess.
  toText (Played (viewAction -> (pid1, Soldier, Guess pid2 card)) NothingHappened) =
    toText pid1 ++ " wrongly guessed " ++ toText pid2 ++ " had a " ++ toText card
    ++ ". Nothing happened, maybe it was the right guess and they were protected."

  -- FIXME: Don't have quite enough information here to disambiguate between
  -- Knight attack failing due to tie and Knight attack failing due to
  -- Priestess.
  toText (Played (viewAction -> (pid1, Knight, Attack pid2)) NothingHappened) =
    toText pid1 ++ " attacked " ++ toText pid2 ++ " with a Knight, but nothing happened. "
    ++ "Because of a bug in the software, you don't know if it's because of a tie or "
    ++ "because " ++ toText pid2 ++ " is protected by the Priestess."

  toText (Played (viewAction -> (pid1, card, Attack pid2)) NothingHappened) =
    toText pid1 ++ " played " ++ toText card ++ " against " ++ toText pid2 ++
    ", but they were protected by the Priestess, so nothing happened"

  toText (Played (viewAction -> (pid1, card, NoEffect)) NothingHappened) =
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

  -- XXX: There are two reasons they could lose here: 1. Discard Prince, 2.
  -- Discard last card.
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


-- | Given the hand of the current player, the hand of the target (if there is
-- one), and the action being played, return the change we need to make.
applyAction' :: Card -> Maybe Card -> Action -> Result
applyAction' _ hand (viewAction -> (_, Soldier, Guess target guess)) =
  if fromJust hand == guess
  then Eliminated target
  else NothingHappened
applyAction' _ (Just targetCard) (viewAction -> (pid, Clown, Attack target)) =
  ForcedReveal pid target targetCard
applyAction' sourceHand targetHand (viewAction -> (pid, Knight, Attack target)) =
  case compare sourceHand (fromJust targetHand) of
    LT -> Eliminated pid
    EQ -> NothingHappened
    GT -> Eliminated target
applyAction' _ _ (viewAction -> (pid, Priestess, NoEffect)) = Protected pid
applyAction' _ hand (viewAction -> (_, Wizard, Attack target)) =
  case hand of
    Just Prince -> Eliminated target
    _ -> ForcedDiscard target
applyAction' _ _ (viewAction -> (pid, General, Attack target)) = SwappedHands target pid
applyAction' _ _ (viewAction -> (_, Minister, NoEffect)) = NothingHappened
applyAction' _ _ (viewAction -> (pid, Prince, NoEffect)) = Eliminated pid
applyAction' _ _ action = error $ "Invalid action: " ++ (show action)


-- | Translate a player action into a change to make to the round.
-- Will return errors if the action is for or against an inactive or
-- nonexistent player.
--
-- If the target player is protected, will return the identity result,
-- NothingHappened.
applyAction :: Round -> Action -> Either BadAction Result
applyAction round action@(viewAction -> (pid, _, play)) = do
  (_, sourceHand) <- getActivePlayerHand round pid
  case getTarget play of
   Nothing -> return $ applyAction' sourceHand Nothing action
   Just target -> do
     (targetPlayer, targetHand) <- getActivePlayerHand round target
     if fromJust (isProtected targetPlayer)
       then return NothingHappened
       else return $ applyAction' sourceHand (Just targetHand) action


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
applyResult round (ForcedReveal _ _ _) = return round



maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a


playTurn :: Round -> Card -> Play -> Either BadAction (Round, Event)
playTurn round chosen play = do
  (playerId, (dealt, hand)) <- maybeToEither RoundOver (currentTurn round)
  player <- getActivePlayer round playerId
  -- The card the player chose is now put in front of them, and the card they
  -- didn't chose is now their hand.
  player' <- maybeToEither (WrongCard chosen (dealt, hand)) (playCard player dealt chosen)
  let round' = replacePlayer (round { _state = Playing }) playerId player'
  if bustingHand dealt hand
    then bustOut round' dealt hand playerId
    else do
      -- An Action is a valid player, card, play combination.
      action <- case playToAction playerId chosen play of
                 Left e -> Left $ InvalidPlay e -- XXX: Bad play. Translate to error type.
                 Right a -> return a
      result <- applyAction round' action
      round'' <- applyResult round' result
      return $ (nextTurn round'', Played action result)


bustOut :: Round -> Card -> Card -> PlayerId -> Either BadAction (Round, Event)
bustOut round dealt hand pid = do
  round' <- adjustPlayer round pid eliminate
  return (nextTurn round', BustedOut pid dealt hand)


data Victory =
  SoleSurvivor PlayerId Card  -- |^ The given player is the only survivor.
  | HighestCard Card [PlayerId] [(PlayerId, Card)]  -- |^  These players have the highest card.
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


instance ConsoleText Victory where
  toText (SoleSurvivor pid card) =
    toText pid ++ " wins as the only remaining player, holding " ++ toText card
  toText (HighestCard card (winner:[]) _) =
    toText winner ++ " wins holding " ++ toText card
  toText (HighestCard card winners _) =
    "Many winners holding " ++ toText card ++ ": " ++ (intercalate ", " (map toText winners))


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
