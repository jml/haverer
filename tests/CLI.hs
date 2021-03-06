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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import BasicPrelude

import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.CLI.CommandLine ()
import Haverer.CLI.Prompt (toText)
import Haverer.Testing (inRoundEvent)


suite :: TestTree
suite = testGroup "Haverer.CLI" [
  testProperty "event toText coverage" $
  forAll inRoundEvent $ not . Text.isPrefixOf "UNKNOWN" . toText
  ]
