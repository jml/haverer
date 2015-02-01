
import Haverer.Action
import Haverer.Deck
import Haverer.Player
import Haverer.Round


main :: IO ()
main = do
  players <- case makePlayerSet 2 of
        Just set -> return set
        Nothing -> fail "Couldn't make set for two players"
  let p1:p2:[] = toPlayers players
  d <- newDeck
  let r = newRound d players
  putStrLn $ show r
  action <- case playToAction p1 Soldier (Guess p2 Wizard) of
    Left e -> fail (show e)
    Right a -> return a
  putStrLn $ show $ applyAction r action
