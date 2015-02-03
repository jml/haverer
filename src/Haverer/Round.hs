module Haverer.Round (newRound, thingy) where

import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Haverer.Action (Action(..), Play, playToAction)
import Haverer.Deck (Card, Complete, Deck, deal, Incomplete, pop)
import Haverer.Player (
  discardAndDraw,
  eliminate,
  getHand,
  newPlayer,
  PlayerId,
  Player,
  PlayerSet,
  protect,
  swapHands,
  toPlayers
  )
import Haverer.Ring (Ring, advance, currentItem, newRing, nextItem)


data Round = Round {
  _stack :: Deck Incomplete,
  _playOrder :: Ring PlayerId,
  _players :: Map.Map PlayerId Player,
  _current :: State
} deriving Show


data State = NotStarted | Turn Card | Over deriving Show

-- XXX: Possibly add burn card to structure?

-- XXX: Write a few invariants.

newRound :: Deck Complete -> PlayerSet -> Round
newRound deck players =
  case deal deck (length playerList) of
   (remainder, Just cards) -> nextTurn $ Round {
     _stack = fst $ pop remainder,
     _playOrder = fromJust (newRing playerList),
     _players = Map.fromList $ zip playerList (map newPlayer cards),
     _current = NotStarted
     }
   _ -> error ("Given a complete deck - " ++ show deck ++ "- that didn't have enough cards for players - " ++ show players)
  where playerList = toPlayers players


drawCard :: Round -> (Round, Maybe Card)
drawCard r =
  let (stack, card) = pop (_stack r) in
  (r { _stack = stack }, card)


-- XXX: Use Ring here and make sure we delete players from it when eliminate
nextPlayer :: Round -> Maybe PlayerId
nextPlayer rnd =
  case _current rnd of
   Over -> Nothing
   NotStarted -> Just $ (currentItem playOrder)
   -- XXX: We want next *active* player
   Turn _ -> Just $ nextItem playOrder
  where playOrder = _playOrder rnd


nextTurn :: Round -> Round
nextTurn r@(Round { _current = Over }) = r
nextTurn r =
  case (drawCard r, nextPlayer r) of
   ((r2, Just card), Just _) -> r2 {
     _current = Turn card,
     _playOrder = advance (_playOrder r)
     }
   _ -> r { _current = Over }



data BadAction = NoSuchPlayer PlayerId | InactivePlayer PlayerId deriving Show

applyAction :: Round -> Action -> Either BadAction Round
applyAction r NoChange = Right r
applyAction r (Protect pid) = adjustPlayer r pid protect
applyAction r (SwapHands pid1 pid2) =
  case (getPlayer r pid1, getPlayer r pid2) of
   (Just p1, Just p2) ->
     case swapHands p1 p2 of
      Nothing -> Left $ InactivePlayer pid1  -- XXX: Not necessarily pid1
      Just (newP1, newP2) -> Right $ (replace pid2 newP2 . replace pid1 newP1) r
   _ -> Left $ NoSuchPlayer pid1  -- XXX: Not necessarily pid1
  where replace pid p rnd = rnd { _players = Map.insert pid p (_players rnd) }
applyAction r (EliminatePlayer pid) = adjustPlayer r pid eliminate
applyAction r (ForceDiscard pid) =
  let (r2, card) = drawCard r in
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


thingy :: Round -> Card -> Play -> Either BadAction (Round, Action)
thingy r chosen play =
  case _current r of
   NotStarted -> thingy (nextTurn r) chosen play
   Over -> error "Don't know how to handle game over"
   Turn _ ->  -- XXX: Need to verify that chosen card is allowed
     let playerId = currentItem (_playOrder r)
         action = playToAction playerId chosen play
     in
      case action of
       Left _ -> error "Bad play"  -- XXX: Bad play. Translate to error type.
       Right a -> fmap (\r2 -> (nextTurn r2, a)) (applyAction r a)


adjustPlayer :: Round -> PlayerId -> (Player -> Maybe Player) -> Either BadAction Round
adjustPlayer rnd pid f =
  case getPlayer rnd pid of
   Nothing -> Left $ NoSuchPlayer pid
   Just player -> case f player of
     Nothing -> Left $ InactivePlayer pid
     Just newP -> Right $ rnd { _players = Map.insert pid newP (_players rnd) }


getPlayer :: Round -> PlayerId -> Maybe Player
getPlayer Round { _players = players } pid = Map.lookup pid players


getPlayerHand :: Round -> PlayerId -> Maybe Card
getPlayerHand r pid = getHand =<< getPlayer r pid

