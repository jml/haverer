import System.IO

import Data.List
import Text.Read
import Text.Show.Pretty

import Haverer.Action
import Haverer.Deck
import Haverer.Player
import Haverer.Round


prompt :: Show e => String -> (String -> Either e a) -> IO (Either e a)
prompt promptStr parser = do
  putStr promptStr
  hFlush stdout
  input <- getLine
  return $ parser input


repeatedlyPrompt :: String -> (String -> Either String a) -> IO a
repeatedlyPrompt promptStr parser = do
  result <- prompt promptStr parser
  case result of
   Left e -> do
     putStrLn e
     repeatedlyPrompt promptStr parser
   Right r -> return r


pickNumPlayers :: IO Int
pickNumPlayers =
  repeatedlyPrompt "Pick number of players: " parseNumPlayers
  where
    parseNumPlayers s =
      case readMaybe s of
       Nothing -> Left errMsg
       Just i -> if 2 <= i && i <= 4 then Right i else Left errMsg
    errMsg = "Please enter a number between 2 and 4"


-- XXX: Actually don't want to allow choosing _any_ card. Want to restrict to
-- cards in hand.
pickCard :: IO Card
pickCard = chooseItem "Please choose a card: " allCards


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
pickTarget ps = chooseItem "Please choose a target: " (toPlayers ps)

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

at :: [a] -> Int -> Maybe a
at xs i = if 0 <= i && i < length xs then Just (xs !! i) else Nothing


-- XXX: Allow specifying base index
chooseItem :: Show a => String -> [a] -> IO a
chooseItem promptStr items =
  repeatedlyPrompt fullPrompt (pickItem items)
  where
    fullPrompt =
      promptStr ++ "\n" ++
      intercalate "\n" [show (i :: Int) ++ ". " ++ show x | (i, x) <- zip [1..] items]
      ++ "\n>>> "
    pickItem xs chosen =
      case readMaybe chosen of
       Nothing -> Left errMsg
       Just i ->
         case xs `at` (i - 1) of
          Nothing -> Left errMsg
          Just x -> return x
    errMsg = "Please select an item from the list"


-- XXX: Say whose turn it is

-- XXX: Better rendering for player ids

-- XXX: Loop until game done

main :: IO ()
main = do
  result <- pickNumPlayers
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail $ "Couldn't make set for " ++ (show result) ++ " players"
  d <- newDeck
  let r = newRound d players
  putStrLn $ ppShow r
  card <- pickCard
  putStrLn $ "You chose: " ++ show card
  play <- pickPlay card players
  putStrLn $ "You chose: " ++ show play
  (r2, a) <- case thingy r card play of
    Left e -> fail (show e)
    Right a -> return a
  putStrLn $ ppShow (r2, a)
