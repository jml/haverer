import System.IO

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


pickNumPlayers :: String -> Either String Int
pickNumPlayers s =
  case readMaybe s of
   Nothing -> Left errMsg
   Just i -> if 2 <= i && i <= 4 then Right i else Left errMsg
  where errMsg = "Please enter a number between 2 and 4"



main :: IO ()
main = do
  result <- repeatedlyPrompt "Pick number of players: " pickNumPlayers
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail $ "Couldn't make set for " ++ (show result) ++ " players"
  let _:p2:[] = toPlayers players
  d <- newDeck
  let r = newRound d players
  putStrLn $ ppShow r
  (r2, a) <- case thingy r Soldier (Guess p2 Wizard) of
    Left e -> fail (show e)
    Right a -> return a
  putStrLn $ ppShow (r2, a)
