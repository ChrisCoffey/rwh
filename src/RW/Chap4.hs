module RW.Chap4 (
  safeHead,
  safeLast,
  safeTail,
  safeInit,
  splitWith,
  firstWords,
  transpose
) where

import Data.Functor
import Data.Char 

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

safeLast :: [a] -> Maybe a
safeLast = foldl (\_ x-> Just x) Nothing

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit ls = Just (go ls)
  where 
    go (_:[]) = []
    go (x:xs) = x:(go xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs =
  let (h, t) = break (not . p) xs
  in h : case t of
    []        -> []
    (_:rest)  -> splitWith p rest

firstWords :: [String] -> [String]
firstWords ls = 
  foldr addWord [] .  map (safeHead . words) $ ls
  where
    addWord Nothing xs = xs
    addWord (Just x) xs = x:xs

--pads with a space when no char found
firstChars :: [[Char]] -> [Char]
firstChars ls = 
  foldr addChar [] . map safeHead $ ls
  where
    addChar Nothing xs = ' ':xs
    addChar (Just c) xs = c:xs

--need to choose whether to pad with empty or truncate
transpose :: [[Char]] ->  [[Char]]
transpose ls 
  | all null ls = []
  | otherwise   =  (firstChars ls):(transpose . map (pTail . safeTail) $ ls)
    where 
      pTail Nothing = []
      pTail (Just t) = t

asInt :: [Char] -> Maybe Int
asInt ('-':rest) = (\x-> -x) <$> (asInt rest)
asInt xs = foldl step (Just 0) xs
  where 
    step Nothing _ = Nothing
    step (Just n) c
      | isDigit c = Just (n *10 + (digitToInt c))
      | otherwise = Nothing

foldCat :: [[a]]-> [a]
foldCat = foldr prep []
  where 
    prep [] acc = acc
    prep (x:xs) acc = x:(prep xs acc)

fTakeWhile :: (a -> Bool) -> [a] -> [a]
fTakeWhile p = foldr pull []
  where 
    pull a acc
      | p a = a:acc
      | otherwise = []

fGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
fGroupBy p = foldr doGroup []
  where 
    doGroup a (h@(x:xs):t)
      | p x a = (a:h):t
      | otherwise = [a]:h:t
    doGroup a acc = [a]:acc

fAny :: (a -> Bool) -> [a] -> Bool
fAny p = foldr (\a b-> b || p a) False

