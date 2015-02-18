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

-- The dreaded 'utils' module of death.
module Utils (
  shuffled,
  iterateM',
  isSubListOf,
  ) where

import Data.List (delete, sort)
import Test.Tasty.QuickCheck
import System.Random.Shuffle (shuffle)


-- | Take a list and generate a shuffled version of it.
shuffled ::[a] -> Gen [a]
shuffled xs = do
  rs <- randomOrdering (length xs - 1)
  return $ shuffle xs rs
  where
    -- a sequence (r1,...r[n-1]) of numbers such that r[i] is an independent
    -- sample from a uniform random distribution [0..n-i]
    randomOrdering 0 = return []
    randomOrdering n =
      do y <- choose (0, n)
         ys <- randomOrdering (n - 1)
         return (y:ys)


-- | Kind of like iterate, but for a monadic function, such that the result of
-- calling once is used as the argument for calling next.
iterateM' :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM' n f x
  | n == 0 = return [x]
  | n > 0  = do y <- f x
                ys <- iterateM' (n - 1) f y
                return (y:ys)
  | otherwise = return []


isSubListOf :: (Eq a, Ord a) => [a] -> [a] -> Bool
isSubListOf xs ys = isSubListOf' (sort xs) (sort ys)

isSubListOf' :: Eq a => [a] -> [a] -> Bool
isSubListOf' (_:_) []  = False
isSubListOf' [] _      = True
isSubListOf' (x:xs) ys =
  if x `elem` ys
  then isSubListOf' xs (delete x ys)
  else False
