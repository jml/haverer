import System.IO

import Text.Read
import Text.Show.Pretty

import Haverer.Action
import Haverer.Deck
import Haverer.Player
import Haverer.Round


repeatedlyPrompt :: Show e => String -> (String -> Either e a) -> IO a
repeatedlyPrompt prompt parser = do
  putStr prompt
  hFlush stdout
  input <- getLine
  case parser input of
   Left e -> do
     putStrLn (show e)
     repeatedlyPrompt prompt parser
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
