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

module Haverer.Internal.Error (
  assertRight,
  assertRight',
  terror
  ) where

import BasicPrelude


-- | Assert that the value is Right. Throws error if it's Left.
assertRight :: (Show a) => Text -> Either a b -> b
assertRight _ (Right value) = value
assertRight message (Left e) = terror $ message ++ show e


-- | Assert that the value is Right. Throws error if it's Left.
assertRight' :: Show a => Either a b -> b
assertRight' (Right value) = value
assertRight' (Left e) = (terror . show) e
