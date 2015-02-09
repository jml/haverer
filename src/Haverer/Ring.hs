module Haverer.Ring (
  Ring
  , newRing
  , advance
  , advance1
  , dropItem
  , dropItem1
  , toList
  , nextItem
  , ringSize
  , currentItem
  ) where


data Ring a = Ring {
  _items :: [a],
  _current :: Int,
  _length :: Int
  } deriving (Show, Eq)


newRing :: [a] -> Maybe (Ring a)
newRing [] = Nothing
newRing xs = Just $ Ring {
  _items = xs,
  _current = 0,
  _length = length xs
  }


toList :: Ring a -> [a]
toList = _items

advanceBy :: Int -> Ring a -> Ring a
advanceBy n ring = ring {
  _current = (_current ring + n) `mod` (_length ring)
  }

advance :: Ring a -> Ring a
advance = advanceBy 1

advance1 :: Ring a -> Either a (Ring a)
advance1 ring =
  case _items ring of
   x:[] -> Left x
   _ -> Right $ advance ring

dropItem :: (Eq a) => Ring a -> a -> Maybe (Ring a)
dropItem ring item =
  case span (/=item) (_items ring) of
   (_, []) -> Just $ ring
   ([], _:[]) -> Nothing
   (pre, _:xs) ->
     let newLength = _length ring - 1
         current = _current ring
     in
     Just $ ring {
       _items = pre ++ xs,
       _length = newLength,
       _current =
         if current == newLength
         then (if null xs then 0 else current - 1)
         else current
     }


dropItem1 :: (Eq a) => Ring a -> a -> Either a (Ring a)
dropItem1 ring item =
  case dropItem ring item of
   Nothing -> Left item
   Just ring' -> Right ring'


ringSize :: Ring a -> Int
ringSize = _length

currentItem :: Ring a -> a
currentItem ring = _items ring !! _current ring

nextItem :: Ring a -> a
nextItem = currentItem . advance

-- XXX: (optional) enforce non-emptyness at the type level
