module Haverer.Player (
  Error,
  discardAndDraw,
  eliminate,
  getDiscards,
  getHand,
  makePlayerSet,
  newPlayer,
  Player,
  PlayerId,
  PlayerSet,
  playCard,
  protect,
  swapHands,
  toPlayers,
  ) where

import Data.List (nub, sort)

import Haverer.Deck (Card(Prince))

newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)


players :: [PlayerId]
players = map PlayerId [1..]


data Error = InvalidNumPlayers Int | DuplicatePlayers


newtype PlayerSet = PlayerSet [PlayerId] deriving (Show)


toPlayers :: PlayerSet -> [PlayerId]
toPlayers (PlayerSet xs) = xs

toPlayerSet :: [PlayerId] -> Either Error PlayerSet
toPlayerSet playerIds =
  if playerIds /= (nub . sort) playerIds
  then Left DuplicatePlayers
       else if numPlayers < 2 || numPlayers > 4
            then Left (InvalidNumPlayers numPlayers)
            else (Right . PlayerSet) playerIds
  where numPlayers = length playerIds


makePlayerSet :: Int -> Maybe PlayerSet
makePlayerSet n =
  case toPlayerSet (take n players) of
   Left _ -> Nothing
   Right playerIds -> Just playerIds


data Player = Active {
  _hand :: Card,
  _protected :: Bool,
  _discard :: [Card]
  } | Inactive [Card] deriving (Show)


newPlayer :: Card -> Player
newPlayer card = Active {
  _hand = card,
  _protected = False,
  _discard = []
  }


operateOn :: (Player -> Player) -> Player -> Maybe Player
operateOn _ (Inactive _) = Nothing
operateOn _ player@(Active _ True _) = Just player
operateOn f player = Just $ f player


protect :: Player -> Maybe Player
protect = operateOn (\p -> p { _protected = True })

eliminate :: Player -> Maybe Player
eliminate = operateOn (\(Active card _ discards) -> Inactive (card:discards))

swapHands :: Player -> Player -> Maybe (Player, Player)
swapHands p1 p2 =
  case (p1, p2) of
   (Active h1 protected _, Active h2 _ _) ->
     if protected
     then Just (p1, p2)
     else Just (p1 { _hand = h2 }, p2 { _hand = h1 })
   _ -> Nothing


discardAndDraw :: Player -> Maybe Card -> Maybe Player
discardAndDraw (Inactive _) _ = Nothing
discardAndDraw (Active card False discards) Nothing = Just $ Inactive (card:discards)
discardAndDraw player@(Active _ True _) _ = Just player
discardAndDraw (Active Prince _ discards) _ = Just $ Inactive (Prince:discards)
discardAndDraw (Active card protected discards) (Just newCard) =
  Just $ Active newCard protected (card:discards)


playCard :: Player -> Card -> Card -> Maybe Player
playCard (Inactive _) _ _ = Nothing
playCard (Active card protected discards) dealt chosen =
  if card == chosen
  then Just $ Active dealt protected (card:discards)
  else if card == dealt
       then Just $ Active card protected (dealt:discards)
       else Nothing

getDiscards :: Player -> [Card]
getDiscards (Inactive ds) = ds
getDiscards (Active _ _ ds) = ds

getHand :: Player -> Maybe Card
getHand (Inactive _) = Nothing
getHand (Active card _ _) = Just card
