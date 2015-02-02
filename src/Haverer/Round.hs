module Haverer.Round (newRound, thingy) where

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


data Round = Round {
  _stack :: Deck Incomplete,
  _playOrder :: [PlayerId],
  _players :: Map.Map PlayerId Player,
  _current :: State
} deriving Show


data State = NotStarted | Turn Int Card | Over deriving Show

-- XXX: Enable complete pattern matching warnings

-- XXX: Possibly add burn card to structure?

-- XXX: Write a few invariants.

newRound :: Deck Complete -> PlayerSet -> Round
newRound deck players =
  case deal deck (length playerList) of
   (remainder, Just cards) -> nextTurn $ Round {
     _stack = fst $ pop remainder,
     _playOrder = playerList,
     _players = Map.fromList $ zip playerList (map newPlayer cards),
     _current = NotStarted
     }
  where playerList = toPlayers players


drawCard :: Round -> (Round, Maybe Card)
drawCard r =
  let (stack, card) = pop (_stack r) in
  (r { _stack = stack }, card)


-- XXX: Use Ring here and make sure we delete players from it when eliminate
nextPlayer :: Round -> Maybe Int
nextPlayer rnd =
  case _current rnd of
   Over -> Nothing
   NotStarted -> Just 0
   -- XXX: We want next *active* player
   Turn i _ -> Just $ (i + 1) `mod` (length playOrder)
  where playOrder = _playOrder rnd


nextTurn :: Round -> Round
nextTurn r@(Round { _current = Over }) = r
nextTurn r =
  case (drawCard r, nextPlayer r) of
   ((r2, Just card), Just i) -> r2 { _current = Turn i card }
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
   Over -> undefined
   Turn i _ ->  -- XXX: Need to verify that chosen card is allowed
     let playerId = (_playOrder r !! i)
         action = playToAction playerId chosen play
     in
      case action of
       Left _ -> undefined  -- XXX: Bad play. Translate to error type.
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

