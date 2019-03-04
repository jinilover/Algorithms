{-# LANGUAGE FlexibleContexts #-}

module DynamicProgramming where

import Protolude hiding (zip, getLine)
import Prelude ((!!), tail, zip, String, readLn, getLine, read, last)
import Data.Text (unpack)
import Control.Monad

-- $setup

-- |
--
-- >>> uniqueList [1,1,2,2,3,4]
-- [1,2,3,4]
--
-- >>> uniqueList [1,2,1,2,3,4,4,3]
-- [1,2,3,4]
uniqueList :: Eq a => [a] -> [a]
uniqueList (x : xs) = x : (uniqueList . filter (x /=) $ xs)
uniqueList xs = xs

nCr :: Eq a => [a] -> Int -> [[a]]
nCr _ 0 = [[]]
nCr [] _ = []
nCr (x : xs) n = [x : ys | ys <- nCr xs (n - 1)] ++ nCr xs n

nPr :: Eq a => [a] -> Int -> [[a]]
nPr _ 0 = [[]]
nPr [] _ = []
nPr xs n = join [ [x : ys | ys <- nPr (filter (x /=) xs) (n - 1)] | x <- xs ]

mergeSort :: Ord a => [a] -> [a]
mergeSort xs@(_ : _ : _) = let (first, second) = splitAt (length xs `div` 2) xs in
                           merge (mergeSort first) (mergeSort second)
                           where merge [] ys = ys
                                 merge xs [] = xs
                                 merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys
mergeSort xs = xs

coinChange :: [Int] -> Int -> [[(Int, Int)]]
coinChange _ 0 = [[]]
coinChange [] _ = []
coinChange (c : cs) sum = concat [ map ((c, size):) $ coinChange cs (sum - c * size) | size <- [1 .. sum `div` c]] ++ coinChange cs sum

fib :: Int -> Int
fib n = fibonacci !! (n - 1)
    where fibonacci = 0 : 1 : [x + y | (x, y) <- zip fibonacci (tail fibonacci)]

pascal :: Int -> [[Int]]
pascal = recur
    where recur 0 = [[1]]
          recur n = let list@(xs : _) = recur (n-1) in
                    ((1:) . snd . foldl (\(prev, ys) x -> (x, prev + x:ys)) (0, [])) xs : list

-- |
-- 1
-- 1 1
-- 1 2 1
-- 1 3 3 1
-- 1 4 6 4 1
-- 
-- >>> pascalTriangle 1
-- [1]
-- 
-- >>> pascalTriangle 2
-- [1,1]
-- 
-- >>> pascalTriangle 3
-- [1,2,1]
-- 
-- >>> pascalTriangle 4
-- [1,3,3,1]
-- 
-- >>> pascalTriangle 5
-- [1,4,6,4,1]
-- 
-- >>> pascalTriangle 6
-- [1,5,10,10,5,1]
pascalTriangle :: Int -> [Int]
pascalTriangle n
    | n <= 1 = [1]
    | otherwise = evalState (traverse toState . pascalTriangle $ n - 1) 0 ++ [1]
        where toState x = state (\s -> (s + x, x))

mingle :: String -> String -> String
mingle xs [] = xs
mingle [] xs = xs
mingle (x:xs) (y:ys) = x : y : mingle xs ys

stringOpermute :: String -> String
stringOpermute [] = []
stringOpermute [x] = [x]
stringOpermute (x1 : x2 : xs) = x2 : x1 : stringOpermute xs

stringCompression :: String -> String
stringCompression [] = []
stringCompression (x : xs) = compress x 1 xs
    where compress lastChar 1 [] = [lastChar]
          compress lastChar n [] = lastChar : show n
          compress lastChar 1 (x : xs) = if lastChar == x then compress lastChar 2 xs else lastChar : compress x 1 xs
          compress lastChar n (x : xs) = if lastChar == x then compress lastChar (n + 1) xs else (lastChar : show n) ++ compress x 1 xs

superDigit :: Int -> Int
superDigit n
    | n `div` 10 == 0 = n
    | otherwise = superDigit . sum . toDigits $ n

toDigits :: Int -> [Int]
toDigits n = case n `div` 10 of
                0 -> [n]
                multiples -> toDigits multiples ++ [n `mod` 10]

-- |
-- >>> isSubstring "abcdef" "def"
-- True
-- 
-- >>> isSubstring "computer" "muter"
-- False
-- 
-- >>> isSubstring "stringmatchingmat" "ingmat"
-- True
-- 
-- >>> isSubstring "videobox" "videobox"
-- True
-- 
isSubstring :: String -> String -> Bool
isSubstring = recur False
    where recur :: Bool -> String -> String -> Bool
          recur _ _ [] = True
          recur _ [] _ = False
          recur False (x : xs) (y : substr) = recur (x == y) xs $ if x == y then substr else y : substr
          recur True (x : xs) (y : substr) = x == y && recur True xs substr

-- |
-- >>> nQueens 1
-- 1
-- 
-- >>> nQueens 2
-- 0
-- 
-- >>> nQueens 3
-- 0
-- 
-- >>> nQueens 4
-- 2
-- 
-- >>> nQueens 5
-- 10
-- 
-- >>> nQueens 6
-- 4
-- 
-- >>> nQueens 10
-- 724
-- 
nQueens :: Int -> Int
nQueens n = length $ recur n
    where recur 0 = [[]]
          recur row = foldl (\lists occupied ->
                        foldl (\lists col ->
                            let pos = (row, col) in 
                            if isSafe pos occupied 
                                then (pos : occupied) : lists 
                                else lists
                        ) lists [1 .. n]
                      ) [] . recur $ row - 1
          isSafe (r, c) xs = all (\(r', c') -> r' /= r && c' /= c) xs && -- neither on occupied row nor col
                             all (notDiag (r,c)) xs -- neither on occupied diagonal
          notDiag (r, c) (r', c') = abs (r - r') /= abs (c - c')

type Interval = (Int, Int)

addInterval :: Interval -> [Interval] -> [Interval]
addInterval tuple@(x, y) xs 
    | any (\(st, ed) -> st <= x && y <= ed) xs = xs
    | otherwise = connect tuple . leftRight tuple $ xs
    where leftRight (x, y) xs = (takeWhile ((< x) . fst) xs, dropWhile ((<= y) . snd) xs)

          connect tuple ([], []) = [tuple]
          connect tuple@(x, y) (xs, []) = let prevElem@(st, ed) = last xs  
                                              newElems 
                                                | x <= ed = [(st, y)]
                                                | ed + 1 == x = [(st, y)]
                                                | otherwise = [prevElem, tuple] in
                                          take (length xs - 1) xs ++ newElems
          connect tuple@(x, y) ([], nextElem@(st, ed) : xs) = let newElems 
                                                                    | st <= y = [(x, ed)]
                                                                    | y + 1 == st = [(x, ed)]
                                                                    | otherwise = [tuple, nextElem] in
                                                              newElems ++ xs
          connect tuple@(x, y) (xs, nextElem@(st2, ed2) : ys) = let prevElem@(st1, ed1) = last xs 
                                                                    newElems
                                                                      | (x <= ed1 || ed1 + 1 == x) && (st2 <= y || y + 1 == st2) = [(st1, ed2)]
                                                                      | (x <= ed1 || ed1 + 1 == x) = [(st1, y), nextElem]
                                                                      | (st2 <= y || y + 1 == st2) = [prevElem, (x, ed2)]
                                                                      | otherwise = [prevElem, tuple, nextElem] in 
                                                                take (length xs - 1) xs ++ newElems ++ ys
            
        