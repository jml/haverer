module Haverer.ValidMoves (getValidMoves) where

import Data.List (delete)
import Prelude hiding (round)

-- XXX: This gives us some idea of what the publicly exposed functions should
-- be.
import Haverer.Action (
  Play(..)
  )
import Haverer.Deck (
  Card(..)
  )
import Haverer.Player (
  PlayerId
  )
import Haverer.Round (
  Round,
  currentTurn,
  getActivePlayers
  )


getValidMoves :: Round -> [(Card, Play)]
getValidMoves round =
  case currentTurn round of
   Nothing -> []
   Just (pid, (dealt, hand)) ->
     [(dealt, play) | play <- getValidPlays pid otherPlayers dealt] ++
     [(hand, play) | play <- getValidPlays pid otherPlayers hand]
     where otherPlayers = delete pid $ getActivePlayers round


-- XXX: This sort of duplicates logic in playToAction.

-- TODO: At least add a test that ensures none of these return BadPlay when
-- applied with playToAction

getValidPlays :: PlayerId -> [PlayerId] -> Card -> [Play]
getValidPlays self others card =
  case card of
   Soldier   -> [Guess tgt c | tgt <- others, c <- [Clown ..]]
   Clown     -> fmap Attack others
   Knight    -> fmap Attack others
   Priestess -> [NoEffect]
   Wizard    -> fmap Attack (self:others)
   General   -> fmap Attack others
   Minister  -> [NoEffect]
   Prince    -> [NoEffect]
