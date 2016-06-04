module RW.Chap3 (
  len,
  lSum,
  lMean
) where


len :: [a] -> Int
len = foldr (\_ a-> a + 1 ) 0

lSum :: [Int] -> Int
lSum = foldr (+) 0

lMean :: [Int] -> Double
lMean xs = s / l
  where 
    l = fromIntegral . len $ xs 
    s = fromIntegral . lSum $ xs

palindromize :: [a] -> [a]
palindromize xs = (\a-> xs ++ a) . reverse $ xs

-- Not efficient, but correct
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

mid = (div 2) . len

qSort :: (Ord a, Eq a) => [a] -> [a]
qSort (a:b:[])
  | a > b     = [b,a]
  | otherwise = [a, b]
qSort (a:[]) = [a]
qSort [] = []
qSort xs = 
  (qSort lt) ++ [pivot] ++ (qSort gt)
  where
    mid = (div 2) . len $ xs
    pivot = head . drop mid $ xs
    lt = filter (< pivot) xs
    gt = filter (> pivot) xs

lSortBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
lSortBy f (a:b:[])
  | f a > f b = [b, a]
  | otherwise = [a, b]
lSortBy _ (a:[]) = [a]
lSortBy _ [] = []
lSortBy f xs = 
  (lSortBy f lt) ++ [pivot] ++ (lSortBy f gt)
  where
    pivot =  head . drop (mid xs) $ xs
    p' = f pivot
    lt = filter (\x-> (f x) < p') xs
    gt = filter (\x-> (f x) > p') xs


