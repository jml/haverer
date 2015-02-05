module Haverer.Prompt where

import Data.List (intercalate)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)


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


at :: [a] -> Int -> Maybe a
at xs i = if 0 <= i && i < length xs then Just (xs !! i) else Nothing


chooseItem :: Show a => String -> [a] -> IO a
chooseItem promptStr items = chooseItem' promptStr 1 items

-- XXX: Crazier: Allow specifying generic Idx
chooseItem' :: Show a => String -> Int -> [a] -> IO a
chooseItem' promptStr startIndex items =
  repeatedlyPrompt fullPrompt (pickItem items)
  where
    fullPrompt =
      promptStr ++ "\n" ++
      intercalate "\n" [show (i :: Int) ++ ". " ++ show x | (i, x) <- zip [startIndex ..] items]
      ++ "\n>>> "
    pickItem xs chosen =
      case readMaybe chosen of
       Nothing -> Left errMsg
       Just i ->
         case xs `at` (i - startIndex) of
          Nothing -> Left errMsg
          Just x -> return x
    errMsg = "Please select an item from the list"
