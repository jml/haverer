
import Haverer.Deck

main :: IO ()
main = do
  d <- newDeck
  putStrLn $ show d
