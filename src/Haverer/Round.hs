module Haverer.Round () where

import qualified Data.Map as Map

import Haverer.Action (Action(..))
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
  _players :: Map.Map PlayerId Player,
  _current :: Maybe PlayerId
}

newRound :: Deck Complete -> PlayerSet -> Round
newRound deck players =
  case deal deck (length playerList) of
   (remainder, Just cards) -> Round {
     _stack = fst $ pop remainder,
     _players = Map.fromList (zip playerList (map newPlayer cards)),
     _current = Just (head playerList)
     }
  where playerList = toPlayers players



data BadAction = NoSuchPlayer PlayerId | InactivePlayer PlayerId

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



replacePlayer :: Round -> PlayerId -> Player -> Either BadAction Round
replacePlayer rnd@(Round { _players = players }) pid newP =
  case Map.lookup pid players of
   Nothing -> Left $ NoSuchPlayer pid
   Just _ -> Right $ rnd { _players = Map.insert pid newP players }


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

drawCard :: Round -> (Round, Maybe Card)
drawCard r =
  let (stack, card) = pop (_stack r) in
  (r { _stack = stack }, card)
