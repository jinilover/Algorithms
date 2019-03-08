module Lists where

import Protolude
import Prelude (tail, id, last)

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ b [] = b
foldLeft f b (x : xs) = foldLeft f (f b x) xs

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ b [] = b
foldRight f b (x : xs) = f x (foldRight f b xs)

-- |
-- >>> allSubLists [1,2,3,4,5,6]
-- [[1],[2],[3],[4],[5],[6],[1,2],[2,3],[3,4],[4,5],[5,6],[1,2,3],[2,3,4],[3,4,5],[4,5,6],[1,2,3,4],[2,3,4,5],[3,4,5,6],[1,2,3,4,5],[2,3,4,5,6],[1,2,3,4,5,6]]
allSubLists :: [a] -> [[a]]
allSubLists xs = join [shiftList size xs | size <- [1 .. (length xs)]]
    where shiftList 0 _ = [[]]
          shiftList _ [] = []
          shiftList size xs@(_ : tl) = let rest = if size > length tl then [] else shiftList size tl
                                           subList = take size xs in
                                       if length subList < size then rest else subList : rest

sublistSum :: [Int] -> Int -> Maybe [Int]
sublistSum list target = increSize 1
  where increSize n
          | n > length list = Nothing
          | otherwise = shiftList list `mplus` increSize (n + 1)

          where shiftList [] = Nothing
                shiftList xs@(_ : tl)
                  | n > length xs = Nothing
                  | otherwise = let subList = take n xs in
                                if sum subList == target then Just subList else shiftList tl

-- |
-- >>> contSublistSize [10, 12, 11]
-- 3
-- 
-- >>> contSublistSize [14, 12, 11, 20]
-- 2
-- 
-- >>> contSublistSize [1, 56, 58, 57, 90, 92, 94, 93, 91, 45]
-- 5
-- 
-- >>> contSublistSize [1,3,5]
-- 1
-- 
-- >>> contSublistSize [1]
-- 1
-- 
-- >>> contSublistSize []
-- 0
-- 
contSublistSize :: [Int] -> Int
contSublistSize [] = 0
contSublistSize xs = maybe 0 id . decreSize (length xs) $ sort xs
    where decreSize n sorted
            | n > 0 = shiftList n sorted `mplus` decreSize (n - 1) sorted
            | otherwise = Nothing

          shiftList n list@(_ : tl) = let subList = take n list
                                          [min', max'] = ($ subList) <$> [minimum, maximum]
                                          listSize = if (max' - min' + 1) == length subList then Just (length subList) else Nothing
                                          nextSubList = if n > length tl then Nothing else shiftList n tl in
                                          listSize `mplus` nextSubList

-- |
-- >>> minSublistGreaterThan [1, 4, 45, 6, 0, 19] 51
-- Just [4,45,6]
-- 
-- >>> minSublistGreaterThan [1, 10, 5, 2, 7] 9
-- Just [10]
-- 
-- >>> minSublistGreaterThan [1, 11, 100, 1, 0, 200, 3, 2, 1, 250] 280
-- Just [100,1,0,200]
-- 
-- >>> minSublistGreaterThan [1, 2, 4] 8
-- Nothing
minSublistGreaterThan :: [Int] -> Int -> Maybe [Int]
minSublistGreaterThan [] _ = Nothing
minSublistGreaterThan xs target = increSize 1
    where increSize n
            | n > length xs = Nothing
            | otherwise = shiftList n xs `mplus` increSize (n + 1)

          shiftList n list@(_ : tl) = let subList = take n list
                                          currSubList = if sum subList > target then Just subList else Nothing
                                          nextSubList = if n > length tl then Nothing else shiftList n tl in
                                          currSubList `mplus` nextSubList

-- |
-- >>> smallestNum [1, 3, 6, 10, 11, 15]
-- 2
-- 
-- >>> smallestNum [1, 1, 1, 1]
-- 5
-- 
-- >>> smallestNum [1, 1, 3, 4]
-- 10
-- 
-- >>> smallestNum [1, 2, 5, 10, 20, 40]
-- 4
-- 
-- >>> smallestNum [1, 2, 3, 4, 5, 6]
-- 22
-- 
-- >>> smallestNum [1, 2, 3, 8]
-- 7
-- 
-- >>> smallestNum [1, 3]
-- 2
-- 
smallestNum :: [Int] -> Int
smallestNum = increment 1 . sort
    where increment :: Int -> [Int] -> Int
          increment acc [] = acc
          increment acc (x : xs) = if acc < x then acc else increment (acc + x) xs