module Haverer.Player (
  Error,
  makePlayerSet,
  PlayerId,
  PlayerSet,
  toPlayers,
  ) where

import Data.List (nub, sort)

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
