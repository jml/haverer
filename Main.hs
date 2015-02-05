import Text.Read
import Text.Show.Pretty

import Haverer.Action
import Haverer.Deck
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


pickCard :: IO Card
pickCard = chooseItem "\nPlease choose a card: " allCards

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
  -- XXX: Exclude Soldier from guess
  guess <- pickGuessCard
  return $ Guess target guess
  where pickGuessCard = pickCard

-- XXX: Exclude self-targeting when it's not legal

-- XXX: Say whose turn it is

-- XXX: Better rendering for player ids


main :: IO ()
main = do
  result <- pickNumPlayers
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail $ "Couldn't make set for " ++ (show result) ++ " players"

  d <- newDeck
  let r = newRound d players

  playRound players r


playRound :: PlayerSet -> Round -> IO ()
playRound players r = do
  result <- playHand players r
  case result of
   Just r' -> do
     putStrLn $ ppShow r'
     playRound players r'
   Nothing -> roundOver r


roundOver :: Round -> IO ()
roundOver r = do
  putStrLn $ "IT IS FINISHED: " ++ show r


playHand :: PlayerSet -> Round -> IO (Maybe Round)
playHand players r =
  case currentHand r of
   Nothing -> return Nothing
   Just hand -> do
     card <- pickCardToPlay hand
     putStrLn $ "You chose: " ++ show card

     play <- pickPlay card players

     (r2, a) <- case thingy r card play of
                 Left e -> fail (show e)
                 Right a -> return a

     putStrLn $ "\nThis amazing thing happened: " ++ show a
     return $ Just r2
