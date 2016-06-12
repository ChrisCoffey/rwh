module RW.Chap6.TCs (
  BasicEq
) where

class BasicEq a where
  isEqual :: a -> a -> Bool
  unEqual :: a -> a -> Bool
  a `unEqual` b = not (isEqual a b)

instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _ _         = False

data Color = 
  Red
  | Blue
  | Green
  | Yellow

instance BasicEq Color where
  isEqual Red Red           = True
  isEqual Yellow Yellow     = True
  isEqual Blue Blue         = True
  isEqual Green Green       = True
  isEqual _ _               = False

instance Show Color where
  show Red = "Red"
  show Yellow = "Yellow"
  show Blue = "Blue"
  show Green = "Green"

instance Read Color where
  readsPrec _ value = 
    tryParse [("Red", Red), ("Blue", Blue), ("Green", Green), ("Yellow", Yellow)]
    where
      tryParse [] = []
      tryParse ((str, val):rest)
        | take (length str) value == str = [(val, drop (length str) value)]
        | otherwise                      = tryParse rest
        

