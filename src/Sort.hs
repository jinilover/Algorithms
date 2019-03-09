module Sort where

import Protolude
import Prelude (last)

mergeSort :: Ord a => [a] -> [a]
mergeSort xs@(_ : _ : _) = let (first, second) = splitAt (length xs `div` 2) xs in
                            merge (mergeSort first) (mergeSort second)
                            where merge [] ys = ys
                                  merge xs [] = xs
                                  merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys
mergeSort xs = xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = recurTake (length xs) xs
    where recurTake n xs
            | n > 0 = recurTake (n - 1) $ recurSort (take n xs) ++ (drop n xs)
            | otherwise = xs
          
          recurSort (x1 : x2 : xs) 
            | x1 < x2 = x1 : recurSort (x2 : xs)
            | otherwise = x2 : recurSort (x1 : xs)
          recurSort xs = xs

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x : xs) = foldl f [x] xs
    where f sorted x = takeWhile (< x) sorted ++ x : dropWhile (< x) sorted

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort (filter (< x) xs) ++ x : quickSort (filter (> x) xs)

-- |
-- >>> searchByList [2,5,8,12,16,23,38,56,72,91] 23
-- Just 5
-- 
-- >>> searchByList [23,38,56,72,91] 23
-- Just 0
-- 
-- >>> searchByList [2,5,8,12,16,23] 23
-- Just 5
-- 
-- >>> searchByList [2,5,8,12,23] 23
-- Just 4
-- 
-- >>> searchByList [2,5,8,12,23] 22
-- Nothing
binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch xs = search (zip [0..(length xs - 1)] xs)
    where search [] _ = Nothing
          search list x = case splitList list of
                            ([], hd : _) -> if snd hd == x then Just $ fst hd else Nothing
                            (xs1, xs2) -> if snd (last xs1) < x then search xs2 x else search xs1 x

          splitList xs = let n = length xs 
                             half = n `div` 2 in 
                         (take half xs, drop half xs)