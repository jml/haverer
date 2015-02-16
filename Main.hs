import Prelude hiding (round)

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.Read

import Haverer.Action
import Haverer.Deck
import qualified Haverer.Game as Game
import Haverer.Player
import Haverer.Prompt
import Haverer.Round


pickNumPlayers :: IO Int
pickNumPlayers =
  repeatedlyPrompt "Pick number of players: " parseNumPlayers
  where
    parseNumPlayers s =
      case readMaybe s of
       Nothing -> Left errMsg
       Just i -> if 2 <= i && i <= 4 then Right i else Left errMsg
    errMsg = "Please enter a number between 2 and 4"


pickCardToPlay :: (Card, Card) -> IO Card
pickCardToPlay (dealt, hand) =
  chooseItem "\nPlease choose a card: " [dealt, hand]


pickPlay :: Card -> PlayerSet -> IO Play
pickPlay card players =
  case card of
   Soldier -> pickGuess players
   Clown -> pickAttack players
   Knight -> pickAttack players
   Priestess -> return NoEffect
   Wizard -> pickAttack players
   General -> pickAttack players
   Minister -> return NoEffect
   Prince -> return NoEffect

pickTarget :: PlayerSet -> IO PlayerId
pickTarget ps = chooseItem "\nPlease choose a target: " (toPlayers ps)

pickAttack :: PlayerSet -> IO Play
pickAttack players = fmap Attack (pickTarget players)

pickGuess :: PlayerSet -> IO Play
pickGuess players = do
  target <- pickTarget players
  guess <- pickGuessCard
  return $ Guess target guess
  where pickGuessCard = chooseItem' "\nWhat card do they have?" 2 [Clown ..]

-- XXX: Exclude self-targeting when it's not legal

-- FIXME: Currently, if you have a "busting" hand with the minister, you still
-- have to select a card to play before you bust out.

main :: IO ()
main = do
  result <- pickNumPlayers
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail $ "Couldn't make set for " ++ (show result) ++ " players"

  let game = Game.newGame players
  putStrLn $ underline '=' "GAME BEGIN"
  putStrLn ""
  outcome <- playGame game
  putStrLn $ "GAME OVER"
  case Game.winners outcome of
   (x:[]) -> putStrLn $ "The winner is: " ++ toText x ++ "!"
   xs -> putStrLn $ "The winners are: " ++ intercalate ", " (map toText xs) ++ "!"
  putStrLn $ formatScores $ Game.finalScores outcome


playGame :: Game.Game -> IO Game.Outcome
playGame game = do
  -- FIXME: Loop these until the *game* is over
  putStrLn $ formatScores $ Game.scores game
  r <- Game.newRound game
  putStrLn $ underline '=' ("ROUND #" ++ show (Game.roundsPlayed game + 1) ++ " BEGIN")
  putStrLn ""
  outcome <- playRound (Game.players game) r
  roundOver outcome
  case Game.playersWon game (getWinners outcome) of
   Left o -> return o
   Right game' -> playGame game'


formatScores :: [(PlayerId, Int)] -> String
formatScores scores =
  underline '-' "Scores" ++ "\n" ++
  unlines (map formatScore scores)
  where formatScore (pid, score) = toText pid ++ ": " ++ toText score


playRound :: PlayerSet -> Round -> IO Victory
playRound players r = do
  putStrLn $ toText r
  result <- playHand players r
  case result of
   Just r' -> do
     playRound players r'
   Nothing -> return $ fromJust $ victory r


roundOver :: Victory -> IO ()
roundOver v = do
  putStrLn $ "ROUND OVER"
  putStrLn $ toText v
  putStrLn ""


getPlay :: PlayerSet -> Round -> (Card, Card) -> IO (Round, Event)
getPlay players round hand = do
  card <- pickCardToPlay hand
  putStrLn $ "You chose: " ++ show card

  play <- pickPlay card players

  case playTurn round card play of
   Left e -> do
     putStrLn $ "ERROR: " ++ (show e)
     getPlay players round hand
   Right a -> return a


playHand :: PlayerSet -> Round -> IO (Maybe Round)
playHand players r =
  case currentTurn r of
   Nothing -> return Nothing
   Just (player, hand) -> do
     putStrLn $ underline '-' $ toText player

     (round', event) <- getPlay players r hand

     putStrLn $ "\n" ++ (toText event) ++ "\n"
     return $ Just round'
