{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Prelude hiding (round)

import Control.Applicative ((<*>), (<$>))
import Data.Maybe (fromJust)

import System.Random.Shuffle (shuffle)
import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Action (Play(..))
import Haverer.Deck (baseCards, Card(..), Complete, Deck, makeDeck)
import Haverer.Player (makePlayerSet, PlayerId, PlayerSet)
import Haverer.Round


instance Arbitrary (Deck Complete) where
  arbitrary = fmap (fromJust . makeDeck) (shuffled baseCards)


shuffled ::[a] -> Gen [a]
shuffled xs = do
  rs <- randomOrdering (length xs - 1)
  return $ shuffle xs rs
  where
    -- a sequence (r1,...r[n-1]) of numbers such that r[i] is an independent
    -- sample from a uniform random distribution [0..n-i]
    randomOrdering 0 = return []
    randomOrdering n =
      do y <- choose (0, n)
         ys <- randomOrdering (n - 1)
         return (y:ys)



instance Arbitrary PlayerSet where
  arbitrary = fmap (fromJust . makePlayerSet) (elements [2, 3, 4])

instance Arbitrary Round where
  arbitrary = newRound <$> arbitrary <*> arbitrary


randomCard :: Round -> Gen (Maybe Card)
randomCard round =
  case currentHand round of
   Just (a, b) -> elements [Just a, Just b]
   Nothing -> return Nothing


randomPlayer :: Round -> Gen PlayerId
randomPlayer = elements . getActivePlayers

randomAttack :: Round -> Gen Play
randomAttack = fmap Attack . randomPlayer

randomAttackOther :: PlayerId -> Round -> Gen Play
randomAttackOther pid round = fmap Attack (randomPlayer round `suchThat` (/= pid))

randomGuess :: PlayerId -> Round -> Gen Play
randomGuess pid round = Guess <$> (randomPlayer round `suchThat` (/= pid)) <*> elements [Clown ..]

randomPlay :: Card -> PlayerId -> Round -> Gen Play
randomPlay Soldier pid round = randomGuess pid round
randomPlay Clown pid round = randomAttackOther pid round
randomPlay Knight pid round = randomAttackOther pid round
randomPlay Priestess _ _ = return NoEffect
randomPlay Wizard _ round = randomAttack round
randomPlay General pid round = randomAttackOther pid round
randomPlay Minister _ _ = return NoEffect
randomPlay Prince _ _ = return NoEffect

-- XXX: I think we want to have a round that's randomly generated from a
-- series of valid moves. I can't think of what benefit there might be from
-- generating _invalid_ moves.

randomCardPlay :: Round -> Gen (Maybe (Card, Play))
randomCardPlay round = do
  case currentTurn round of
   Nothing -> return Nothing
   Just (pid, (dealt, chosen)) -> do
     card <- elements [dealt, chosen]
     play <- randomPlay card pid round
     return $ Just $ (card, play)


applyPlay :: Round -> Maybe (Card, Play) -> Either BadAction Round
applyPlay r Nothing = return r
applyPlay r (Just (card, play)) = fmap fst (thingy r card play)

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error $ "unexpected Left " ++ show a


randomNextMove :: Round -> Gen Round
randomNextMove r = fmap (fromRight . applyPlay r) (randomCardPlay r)


iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f = iterate (f =<<) . return


twoConsecutiveRounds :: Gen (Round, Round)
twoConsecutiveRounds = do
  r1 <- arbitrary
  r2 <- randomNextMove r1
  return (r1, r2)


nextPlayerNeverCurrentPlayer :: Round -> Bool
nextPlayerNeverCurrentPlayer round =
  currentPlayer round /= nextPlayer round || currentPlayer round == Nothing


-- FIXME: Priestess never expires. Add a property that when it's your turn you
-- are never protected.

-- XXX: Do random [0..15] for number of moves, so we exercise more of the
-- tree, then ditch 'after one move' bollocks.

suite :: TestTree
suite = testGroup "Haverer.Round" [
  testGroup "QuickCheck tests"
  [ testProperty "allCardsPresent" prop_allCardsPresent
  , testProperty "allCardsPresent after move" $
    forAll (arbitrary >>= randomNextMove) prop_allCardsPresent
  , testProperty "allCardsPresent after many moves" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all prop_allCardsPresent
  , testProperty "next player is not current player" nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after turn"
    $ forAll (arbitrary >>= randomNextMove) nextPlayerNeverCurrentPlayer
  , testProperty "next player is not current player after many moves" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all nextPlayerNeverCurrentPlayer
  , testProperty "ring is active players" $ prop_ringIsActivePlayers
  , testProperty "ring is active players after move" $
    forAll (arbitrary >>= randomNextMove) prop_ringIsActivePlayers
    -- XXX: Update prop_burnCardsSame to take a list, then delete twoConsecutiveRounds
  , testProperty "burn card same after move" $
    forAll twoConsecutiveRounds $ \(x, y) -> prop_burnCardsSame x y
  , testProperty "multiple active players or over" $
    forAll (arbitrary >>= randomNextMove) prop_multipleActivePlayers
  , testProperty "multiple active players or over after many moves" $
    forAll (arbitrary >>= sequence . take 5 . iterateM randomNextMove) $ all prop_multipleActivePlayers
  ]
 ]
