module Haverer.Round (allCardsPresent
                     , BadAction
                     , currentHand
                     , currentPlayer
                     , currentTurn
                     , getPlayers
                     , newRound
                     , nextPlayer
                     , nextTurn -- XXX: exported only for tests
                     , Round
                     , thingy) where

import Data.Maybe (fromJust, isJust, maybeToList)
import qualified Data.Map as Map

import Haverer.Action (Action(..), BadPlay, Play, playToAction)
import Haverer.Deck (Card, Complete, Deck, deal, Incomplete, pop)
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
  toPlayers
  )
import Haverer.Ring (Ring, advance, currentItem, dropItem1, newRing, nextItem)


data Round = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring PlayerId,
  _players :: Map.Map PlayerId Player,
  _current :: State,
  _burn :: Card
} deriving Show


data State = NotStarted | Turn Card | Over deriving Show

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
        _current = NotStarted,
        _burn = burn
        }
   _ -> error ("Given a complete deck - " ++ show deck ++ "- that didn't have enough cards for players - " ++ show players)
  where playerList = toPlayers players


getPlayers :: Round -> [PlayerId]
getPlayers = Map.keys . _players


drawCard :: Round -> (Round, Maybe Card)
drawCard r =
  let (stack, card) = pop (_stack r) in
  (r { _stack = stack }, card)


currentPlayer :: Round -> Maybe PlayerId
currentPlayer rnd =
  case _current rnd of
   Over -> Nothing
   NotStarted -> Nothing
   Turn _ -> Just $ (currentItem . _playOrder) rnd


currentHand :: Round -> Maybe (Card, Card)
currentHand rnd = do
  pid <- currentPlayer rnd
  hand <- getPlayerHand rnd pid
  case _current rnd of
   Turn dealt -> return (dealt, hand)
   _ -> Nothing


currentTurn :: Round -> Maybe (PlayerId, (Card, Card))
currentTurn rnd = do
  pid <- currentPlayer rnd
  cards <- currentHand rnd
  return (pid, cards)


nextPlayer :: Round -> Maybe PlayerId
nextPlayer rnd =
  case _current rnd of
   Over -> Nothing
   NotStarted -> Just $ (currentItem playOrder)
   Turn _ -> Just $ nextItem playOrder
  where playOrder = _playOrder rnd


-- XXX: Would using a State monad make any of this code better?


nextTurn :: Round -> Round
nextTurn r@(Round { _current = Over }) = r
-- XXX: This is kind of crap. Either eliminate the duplication using some kind
-- of composable operation, or just get rid of the NotStarted state, since we
-- basically don't use it.
nextTurn r@(Round { _current = NotStarted }) =
  case drawCard r of
   (r2, Just card) -> r2 { _current = Turn card }
   _ -> error $ "Cannot draw a card in just started round: " ++ show r
nextTurn r =
  case (drawCard r, nextPlayer r) of
   ((r2, Just card), Just _) -> r2 {
     _current = Turn card,
     -- XXX: Should be advance1
     -- XXX: Types should have prevented me from doing this
     _playOrder = advance (_playOrder r)
     }
   _ -> r { _current = Over }



data BadAction = NoSuchPlayer PlayerId
               | InactivePlayer PlayerId
               | InvalidPlay BadPlay
               | WrongCard Card (Card, Card)
               deriving Show

applyAction :: Round -> Action -> Either BadAction Round
applyAction r NoChange = Right r
applyAction r (Protect pid) = adjustPlayer r pid protect
-- XXX: This is buggy. When swapping hands and the General happens to be 'in
-- the hand', then the target player gets the general. Instead, they should
-- get the dealt card.
applyAction r (SwapHands pid1 pid2) =
  case (getPlayer r pid1, getPlayer r pid2) of
   (Just p1, Just p2) ->
     case swapHands p1 p2 of
      Nothing -> Left $ InactivePlayer pid1  -- XXX: Not necessarily pid1
      Just (newP1, newP2) -> Right $ (replace pid2 newP2 . replace pid1 newP1) r
   _ -> Left $ NoSuchPlayer pid1  -- XXX: Not necessarily pid1
  where replace pid p rnd = replacePlayer rnd pid p
applyAction r (EliminatePlayer pid) = adjustPlayer r pid eliminate
applyAction r (ForceDiscard pid) =
  let (r2, card) = drawCard r in
  -- XXX: Does being forced to discard the princess end the round?
  adjustPlayer r2 pid (flip discardAndDraw card)
applyAction r (ForceReveal _ _) = Right r
applyAction r (EliminateWeaker pid1 pid2) =
  case (getPlayerHand r pid1, getPlayerHand r pid2) of
   (Just c1, Just c2) ->
     case compare c1 c2 of
      LT -> adjustPlayer r pid1 eliminate
      EQ -> Right r
      GT -> adjustPlayer r pid2 eliminate
   (Nothing, Just _) -> Left $ InactivePlayer pid1
   (_, Nothing) -> Left $ InactivePlayer pid2
applyAction r (EliminateOnGuess pid guess) =
  adjustPlayer r pid $ \p ->
  case getHand p of
   Nothing -> Nothing
   Just card -> if card == guess then eliminate p else Just p


-- XXX: No way to send Clown / ForceReveal results
-- XXX: No way for communicating busting out due to minister
-- XXX: End round when only one player left
-- XXX: End round when no cards left
-- XXX: 'thingy' is a terrible name


-- XXX: If player plays Priestess, is protected forever

thingy :: Round -> Card -> Play -> Either BadAction (Round, Action)
thingy r chosen play =
  case currentTurn r of
   Nothing -> error $ "Trying to play turn in game that's over or hasn't started: " ++ show r
   Just (playerId, (dealt, hand)) ->
     case other chosen (dealt, hand) of
      Nothing -> Left $ WrongCard chosen (dealt, hand)
      Just _ ->
         case playToAction playerId chosen play of
          Left e -> Left $ InvalidPlay e  -- XXX: Bad play. Translate to error type.
          -- XXX: Discard the card that was actually played
          Right a -> do
            -- XXX: Now we're here, we know the player can play the card.
            --
            -- We need to
            --   a) make sure the players hand is whatever of (dealt, hand)
            --      they didn't play
            --   b) add whatever they did play to ther discards
            --   c) hope that having a card duplicated in Turn & their hand
            --      isn't such a bad thing
            --   d) perform the action
            --   e) advance the play order to the next player
            --   f) draw a card from the top of the stack
            --
            -- nextTurn is definitely losing a card somewhere. 
            r2 <- adjustPlayer r playerId (\p -> playCard p dealt chosen)
            r3 <- applyAction r2 a
            return (nextTurn r3, a)


other :: Eq a => a -> (a, a) -> Maybe a
other x (y, z)
  | x == z = Just y
  | x == y = Just z
  | otherwise = Nothing


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
       Left _ -> rnd { _current = Over }
       Right newOrder -> newRnd { _playOrder = newOrder }


getPlayer :: Round -> PlayerId -> Maybe Player
getPlayer Round { _players = players } pid = Map.lookup pid players


getPlayerHand :: Round -> PlayerId -> Maybe Card
getPlayerHand r pid = getHand =<< getPlayer r pid


-- Return all the cards in the round. Intended for testing.
allCardsPresent :: Round -> Bool
allCardsPresent =
  isJust . Deck.makeDeck . allCards
  where allCards rnd =
          _burn rnd : (
            (Deck.toList . _stack) rnd
            ++ (concatMap getDiscards . Map.elems . _players) rnd
            ++ (concatMap (maybeToList . getHand) . Map.elems . _players) rnd)
            ++ (
            case _current rnd of
              Turn x -> [x]
              _ -> [])

-- XXX: Property that burn card is the same

-- XXX: Property that active players == ring

-- XXX: Property that !Over ==> more than one active player

-- XXX: Property that later deck is smaller than earlier deck
