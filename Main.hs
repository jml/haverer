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


repeatedlyPrompt :: Show e => String -> (String -> Either e a) -> IO a
repeatedlyPrompt promptStr parser = do
  result <- prompt promptStr parser
  case result of
   Left e -> do
     putStrLn (show e)
     repeatedlyPrompt promptStr parser
   Right r -> return r


pickNumber :: String -> Either String Int
pickNumber = readEither



main :: IO ()
main = do
  result <- repeatedlyPrompt "Pick number of players: " pickNumber
  players <- case makePlayerSet result of
        Just set -> return set
        Nothing -> fail "Couldn't make set for two players"
  let _:p2:[] = toPlayers players
  d <- newDeck
  let r = newRound d players
  putStrLn $ ppShow r
  (r2, a) <- case thingy r Soldier (Guess p2 Wizard) of
    Left e -> fail (show e)
    Right a -> return a
  putStrLn $ ppShow (r2, a)
