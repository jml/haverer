module Haverer.Ring (
  Ring
  , newRing
  , advance
  , dropItem
  , nextItem
  , ringSize
  , currentItem
  ) where


data Ring a = Ring {
  _items :: [a],
  _current :: Int,
  _length :: Int
  } deriving Show


newRing :: [a] -> Ring a
newRing [] = error "Must not be empty"
newRing xs = Ring {
  _items = xs,
  _current = 0,
  _length = length xs
  }


advanceBy :: Int -> Ring a -> Ring a
advanceBy n ring = ring {
  _current = (_current ring + n) `mod` (_length ring)
  }

advance :: Ring a -> Ring a
advance = advanceBy 1

dropItem :: (Eq a) => Ring a -> a -> Ring a
dropItem ring item =
  case span (/=item) (_items ring) of
   (pre, _:xs) ->
     let newLength = _length ring - 1
         current = _current ring
     in
     ring {
       _items = pre ++ xs,
       _length = newLength,
       _current =
         if current == newLength
         then (if null xs then 0 else current - 1)
         else current
     }
   (_, []) -> ring

ringSize :: Ring a -> Int
ringSize = _length

currentItem :: Ring a -> a
currentItem ring = _items ring !! _current ring

nextItem :: Ring a -> a
nextItem = currentItem . advance

-- XXX: (optional) enforce non-emptyness at the type level

-- XXX: tests!
