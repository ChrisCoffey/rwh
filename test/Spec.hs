import Test.QuickCheck

import RW.Chap3

main :: IO ()
main = quickCheck prop_length

prop_length :: [Int] -> Bool
prop_length xs = length xs == len xs

prop_sum :: [Int] -> Bool
prop_sum xs = sum xs == lSum xs

