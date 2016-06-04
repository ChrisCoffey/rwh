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

lIntersperce :: a -> [[a]] -> [a]
lIntersperce sep (l:[]) = l
lIntersperce sep (l:ls) = l ++ [sep] ++ (lIntersperce sep ls)

data Tree a = Empty | Node a (Tree a) (Tree a)

simpleTree = Node 1 (Node 2 (Node 1 Empty Empty) Empty) Empty

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + (max (treeHeight l) (treeHeight r))

data Direction = DLeft | DRight | DStraight 
type Point = (Double, Double)

-- I suspect this is wrong once we pass through 90 degrees
findBend :: Point -> Point -> Point -> Direction
findBend a b c = 
  case vProd a b c of
    x
      | 0     -> DStraight
      | x < 0 -> DRight
      | x > 0 -> DLeft
  where
    vProd (x,y) (x', y') (x'', y'') =
      (x' - x)*(y'' - y) - (y' - y)*(x'' - x)

findBends :: [Point] -> [Direction]
findBends (a:b:[]) = []
findBends (a:b:c:xs) = 
  (findBend a b c) : findBends (b:c:xs)

type Hull = [Point]
convexHull :: [Point] -> Hull
convexHull xs = 
  where
    sorted = lSortBy (snd) xs
    
