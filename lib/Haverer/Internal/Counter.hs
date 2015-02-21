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

module Haverer.Internal.Counter (
  Counter,
  increment,
  incrementMany,
  initialize,
  toList,
  top,
  topValue,
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map


data Counter k a = Counter { _uncounter :: Map.Map k a } deriving Show

incrementMany :: (Ord k, Num a, Foldable.Foldable m) => Counter k a -> m k -> Counter k a
incrementMany counter keys = Foldable.foldl increment counter keys

increment :: (Ord k, Num a) => Counter k a -> k -> Counter k a
increment (Counter m) key =
  Counter $ Map.insertWith (+) key 1 m


initialize :: (Ord k, Num a) => [k] -> Counter k a
initialize keys = Counter $ Map.fromList (zip keys (repeat 0))

topValue :: (Ord k, Ord a) => Counter k a -> a
topValue (Counter m) = Foldable.maximum $ Map.elems m

withValue :: (Ord k, Eq a) => Counter k a -> a -> [k]
withValue (Counter m) v  = Map.keys $ Map.filter (== v) m

top :: (Ord k, Ord a, Eq a) => Counter k a -> (a, [k])
top counter = let v = topValue counter in (v, withValue counter v)

toList :: (Ord k) => Counter k a -> [(k, a)]
toList = Map.toList . _uncounter
