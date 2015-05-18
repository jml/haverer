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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Haverer.CLI.Prompt where

import BasicPrelude

import Control.Error hiding (readMay)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)


class ConsoleText a where
  toText :: a -> Text

instance ConsoleText Text where
  toText = id

instance ConsoleText Int where
  toText = show


underline :: Char -> Text -> Text
underline char string = string ++ Text.pack ('\n':replicate (Text.length string) char)


prompt :: ConsoleText e => Text -> (Text -> Either e a) -> IO (Either e a)
prompt promptStr parser = do
  putStr promptStr
  hFlush stdout
  input <- getLine
  return $ parser input


repeatedlyPrompt :: ConsoleText e => Text -> (Text -> Either e a) -> IO a
repeatedlyPrompt promptStr parser = do
  result <- prompt promptStr parser
  case result of
   Left e -> do
     putStrLn $ toText e
     repeatedlyPrompt promptStr parser
   Right r -> return r


chooseItem :: ConsoleText a => Text -> [a] -> IO a
chooseItem promptStr = chooseItem' promptStr 1

-- XXX: Crazier: Allow specifying generic Idx
chooseItem' :: ConsoleText a => Text -> Int -> [a] -> IO a
chooseItem' promptStr startIndex items =
  repeatedlyPrompt fullPrompt (pickItem items)
  where
    fullPrompt =
      promptStr ++ "\n" ++
      intercalate "\n" [toText (i :: Int) ++ ". " ++ toText x | (i, x) <- zip [startIndex ..] items]
      ++ "\n>>> "
    pickItem xs chosen = note errMsg $ do
      i <- readMay chosen
      xs `atMay` (i - startIndex)
    errMsg = "Please select an item from the list" :: Text
