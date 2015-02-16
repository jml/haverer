-- Copyright (c) 2014-2015 Jonathan M. Lange <jml@mumak.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Haverer.Prompt where

import Data.List (intercalate)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)


class ConsoleText a where
  toText :: a -> String

instance ConsoleText String where
  toText = id

instance ConsoleText Int where
  toText = show


underline :: Char -> String -> String
underline char string = string ++ '\n':take (length string) (repeat char)


prompt :: ConsoleText e => String -> (String -> Either e a) -> IO (Either e a)
prompt promptStr parser = do
  putStr promptStr
  hFlush stdout
  input <- getLine
  return $ parser input


repeatedlyPrompt :: ConsoleText e => String -> (String -> Either e a) -> IO a
repeatedlyPrompt promptStr parser = do
  result <- prompt promptStr parser
  case result of
   Left e -> do
     putStrLn $ toText e
     repeatedlyPrompt promptStr parser
   Right r -> return r


at :: [a] -> Int -> Maybe a
at xs i = if 0 <= i && i < length xs then Just (xs !! i) else Nothing


chooseItem :: ConsoleText a => String -> [a] -> IO a
chooseItem promptStr items = chooseItem' promptStr 1 items

-- XXX: Crazier: Allow specifying generic Idx
chooseItem' :: ConsoleText a => String -> Int -> [a] -> IO a
chooseItem' promptStr startIndex items =
  repeatedlyPrompt fullPrompt (pickItem items)
  where
    fullPrompt =
      promptStr ++ "\n" ++
      intercalate "\n" [toText (i :: Int) ++ ". " ++ toText x | (i, x) <- zip [startIndex ..] items]
      ++ "\n>>> "
    pickItem xs chosen =
      case readMaybe chosen of
       Nothing -> Left errMsg
       Just i ->
         case xs `at` (i - startIndex) of
          Nothing -> Left errMsg
          Just x -> return x
    errMsg = "Please select an item from the list"
