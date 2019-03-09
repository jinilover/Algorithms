{-# LANGUAGE GADTs
           , StandaloneDeriving
           , RecordWildCards #-}

module Trees where

import Protolude 
import Prelude (tail, String, read, id)
import qualified Data.Vector as V
import qualified Data.Map as M

-- $setup

data Tree a where
    Empty :: Tree a
    Branch :: Tree a -> a -> Tree a -> Tree a

deriving instance Show a => Show (Tree a)
deriving instance Eq a => Eq (Tree a)

-- instance Eq a => Eq (Tree a) where
--   (==) (Branch l1 x1 r1) (Branch l2 x2 r2) = and [l1 == l2, x1 == x2, r1 == r2]
--   (==) Empty Empty = True
--   (==) _ _ = False

-- |
-- >>> isBstPreorder "1 2 3"
-- True
-- 
-- >>> isBstPreorder "2 1 3"
-- True
-- 
-- >>> isBstPreorder "3 2 1 5 4 6"
-- True
-- 
-- >>> isBstPreorder "1 3 4 2"
-- False
-- 
-- >>> isBstPreorder "3 4 5 1 2"
-- False
-- 
isBstPreorder :: String -> Bool
isBstPreorder = isIntsPreorder . map (\a -> read a :: Int) . tokenise ' '

isIntsPreorder :: Ord a => [a] -> Bool
isIntsPreorder xs@(x : _ : _) = let (less, greater) = split (< x) . tail $ xs in
                                and [all (< x) less, all (x <) greater, isIntsPreorder less, isIntsPreorder greater]
isIntsPreorder _ = True

split :: Ord a => (a -> Bool) -> [a] -> ([a], [a])
split f = liftM2 (,) (takeWhile f) (dropWhile f)

tokenise :: Char -> String -> [String]
tokenise = flip recur ""
  where recur _ [] [] = []
        recur _ token [] = [token]
        recur delim token (x : xs) = if delim == x
                                        then token : recur delim "" xs
                                        else recur delim (token ++ [x]) xs

-- |
-- >>> isBst $ Branch (Branch Empty 7 (Branch Empty 11 Empty)) 10 (Branch Empty 39 Empty)
-- False
-- 
-- isBst $ Branch (Branch E) 1 (Branch Empty 2 Empty)
-- False
-- 
-- isBst $ Branch (Branch (Branch Empty 40 Empty) 20 (Branch Empty 60 Empty)) 10 (Branch Empty 30 Empty)
-- False
-- 
-- isBst $ Branch (Branch Empty 1 Empty) 2 (Branch Empty 3 Empty)
-- True
-- 
-- isBst $ Branch (Branch (Branch Empty 1 Empty) 2 Empty) 3 (Branch (Branch Empty 4 Empty) 5 (Branch Empty 6 Empty))
-- True
isBst :: Tree Int -> Bool
isBst = isSubTreeBst . recur
  where recur = foldTree f (IsBstInfo Nothing True Nothing)

        f (IsBstInfo minL bL maxL) x (IsBstInfo minR bR maxR) = 
          let isSubTreeBst = and [bL, bR, lessThan maxL (Just x), lessThan (Just x) minR]
              [subtreeMin, subtreeMax] = (\g -> Just . g . catMaybes $ [minL, maxL, Just x, minR, maxR]) <$> 
                                         [minimum, maximum] in
          IsBstInfo {..}

        lessThan x y = maybe True id $ x >>= (\v -> (v <) <$> y)
    
data IsBstInfo = IsBstInfo {
    subtreeMin :: Maybe Int
  , isSubTreeBst :: Bool
  , subtreeMax :: Maybe Int
}

-- |
-- >>> preorderStringToBst "3 1 2"
-- Branch (Branch Empty 1 (Branch Empty 2 Empty)) 3 Empty
-- 
-- >>> preorderStringToBst "3 2 1 4 6 5"
-- Branch (Branch (Branch Empty 1 Empty) 2 Empty) 3 (Branch Empty 4 (Branch (Branch Empty 5 Empty) 6 Empty))
preorderStringToBst :: String -> Tree Int
preorderStringToBst = intsToBst . map (\a -> read a :: Int) . tokenise ' '
    where intsToBst [] = Empty
          intsToBst (x : xs) = case split (< x) xs of
                                ([], []) -> Branch Empty x Empty
                                ([], ys) -> Branch Empty x (intsToBst ys)
                                (xs, []) -> Branch (intsToBst xs) x Empty
                                (xs, ys) -> Branch (intsToBst xs) x (intsToBst ys)

-- |
-- >>> (bstToPreorderString $ preorderStringToBst "3 1 2") == "3 1 2"
-- True
-- 
-- >>> (bstToPreorderString $ preorderStringToBst "3 2 1 4 6 5") == "3 2 1 4 6 5"
-- True
bstToPreorderString :: (Show a, Ord a) => Tree a -> String
bstToPreorderString = intercalate " " . map show . foldTree_preorder

-- | 
-- it can cope with rightMost or leftMost
-- due to b -> a -> b -> b
-- b must be 0 for summation
-- if the ADT has 3 data/data constructors, it should be 
-- foldTree :: (b -> a -> b -> b) -> (a -> b) -> b -> Tree a -> b
foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree _ b Empty = b
foldTree f b (Branch lt x rt) = f (foldTree f b lt) x (foldTree f b rt)

-- |
-- >>> foldTree_rightMost $ Branch (Branch Empty 1 (Branch Empty 2 Empty)) 3 Empty
-- Just 3
-- 
-- >>> foldTree_rightMost $ Branch Empty 1 (Branch Empty 2 (Branch Empty 4 (Branch (Branch Empty 3 Empty) 5 Empty)))
-- Just 5
-- 
-- >>> foldTree_rightMost $ Branch (Branch (Branch Empty 1 Empty) 2 Empty) 3 (Branch Empty 4 (Branch (Branch Empty 5 Empty) 6 Empty))
-- Just 6
foldTree_rightMost :: Tree a -> Maybe a
foldTree_rightMost = foldTree (const $ flip mplus . Just) Nothing

-- |
-- >>> foldTree_leftMost $ Branch (Branch Empty 1 (Branch Empty 2 Empty)) 3 Empty
-- Just 1
-- 
-- >>> foldTree_leftMost $ Branch Empty 1 (Branch Empty 2 (Branch Empty 4 (Branch (Branch Empty 3 Empty) 5 Empty)))
-- Just 1
foldTree_leftMost :: Tree a -> Maybe a
foldTree_leftMost = foldTree (\lt x _ -> mplus lt $ Just x) Nothing

-- |
-- >>> foldTree_inorder $ Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 Empty)
-- [4,2,5,1,3]
foldTree_inorder :: Tree a -> [a]
foldTree_inorder = foldTree (\lb x rb -> lb ++ x : rb) []

-- |
-- >>> foldTree_preorder $ Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 Empty)
-- [1,2,4,5,3]
-- 
foldTree_preorder :: Tree a -> [a]
foldTree_preorder = foldTree (\lb x rb -> x : lb ++ rb) []

-- |
-- >>> foldTree_postorder $ Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 Empty)
-- [4,5,2,3,1]
-- 
foldTree_postorder :: Tree a -> [a]
foldTree_postorder = foldTree (\lb x rb -> lb ++ rb ++ [x]) []

-- |
-- >>> foldTree_levelorder $ Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 Empty)
-- [1,2,3,4,5]
-- 
-- >>> foldTree_levelorder $ Branch (Branch Empty 3 Empty) 1 (Branch Empty 2 Empty)
-- [1,3,2]
-- 
-- >>> foldTree_levelorder $ Branch (Branch (Branch Empty 40 Empty) 20 (Branch Empty 60 Empty)) 10 (Branch Empty 30 Empty)
-- [10,20,30,40,60]
foldTree_levelorder :: Tree a -> [a]
foldTree_levelorder = breadthFirstBst -- level order is indeed breadth first

-- |
-- >>> foldTree_minDepth $ Branch (Branch Empty 3 Empty ) 1 (Branch Empty 2 Empty)
-- 2
-- 
-- >>> foldTree_minDepth $ Branch (Branch (Branch Empty 40 Empty) 20 (Branch Empty 60 Empty)) 10 (Branch Empty 30 Empty)
-- 2
-- 
-- >>> foldTree_minDepth Empty
-- 0
-- 
-- >>> foldTree_minDepth $ Branch Empty 3 Empty
-- 1
-- 
-- >>> foldTree_minDepth $ Branch Empty 1 (Branch Empty 2 (Branch Empty 4 (Branch (Branch Empty 3 Empty) 5 Empty)))
-- 5
-- 
-- >>> foldTree_minDepth $ Branch (Branch (Branch Empty 1 Empty) 2 Empty) 3 (Branch Empty 4 (Branch (Branch Empty 5 Empty) 6 Empty))
-- 3
foldTree_minDepth :: Tree a -> Int
foldTree_minDepth = foldTree f 0
    where f 0 _ minRt = 1 + minRt
          f minLt _ 0 = 1 + minLt
          f minLt _ minRt = 1 + min minLt minRt

-- |
-- >>> foldTree_maxPathSum $ Branch (Branch Empty 2 Empty) 1 (Branch Empty 3 Empty)
-- 6
-- 
-- >>> foldTree_maxPathSum $ Branch (Branch (Branch Empty 20 Empty) 2 (Branch Empty 1 Empty)) 10 ((Branch Empty 10 ((Branch (Branch Empty 3 Empty) (-25) (Branch Empty 4 Empty)))))
-- 42
-- 
-- >>> foldTree_maxPathSum $ Branch (Branch (Branch Empty 20 Empty) 2 (Branch Empty 1 Empty)) (-10) ((Branch Empty 10 ((Branch (Branch Empty 3 Empty) (-25) (Branch Empty 4 Empty)))))
-- 23
-- 
-- >>> foldTree_maxPathSum $ Branch (Branch (Branch Empty (-20) Empty) (-2) (Branch Empty (-1) Empty)) (-10) ((Branch Empty (-10) ((Branch (Branch Empty (-3) Empty) (-25) (Branch Empty (-4) Empty)))))
-- -1
-- 
-- >>> foldTree_maxPathSum $ Branch (Branch (Branch Empty (-20) Empty) (-2) (Branch Empty (-1) Empty)) (-10) ((Branch Empty (-10) ((Branch (Branch Empty (-3) Empty) (25) (Branch Empty (-4) Empty)))))
-- 25
-- 
-- >>> foldTree_maxPathSum $ Branch (Branch (Branch Empty 20 Empty) 2 (Branch Empty 1 Empty)) (-10) ((Branch Empty 10 ((Branch (Branch Empty 3 Empty) 25 (Branch Empty 100 Empty)))))
-- 147
-- 
foldTree_maxPathSum :: Tree Int -> Int
foldTree_maxPathSum = (\(PathSumInfo tSums cSums) -> maximum $ tSums ++ cSums) . allPathSums
    where allPathSums = foldTree f (PathSumInfo [] [])
    
          f (PathSumInfo lTSums lCSums) x (PathSumInfo rTSums rCSums) =
            let extendableSums = x : map (+x) (lCSums ++ rCSums)
                terminatedSums = lTSums ++ rTSums ++ extendableSums ++ 
                    do
                        lCSum <- lCSums
                        rCSum <- rCSums
                        return (lCSum + x + rCSum) in
            PathSumInfo {..}

data PathSumInfo = PathSumInfo {
    terminatedSums :: [Int] -- sums that cannot be continued
  , extendableSums :: [Int] -- sums that can be continued
}

-- |
-- e.g.
--     a
--    / \
--   b   c
--  /   / \
-- d   e   f
-- 
-- should be a,b,d,c,e,f
-- 
-- >>> depthFirstBst $ Branch ((Branch (Branch Empty 'd' Empty) 'b' Empty)) 'a' (Branch (Branch Empty 'e' Empty) 'c' (Branch Empty 'f' Empty))
-- "abdcef"
-- 
-- >>> depthFirstBst $ Branch (Branch (Branch (Branch Empty 'h' Empty) 'd' (Branch Empty 'i' Empty)) 'b' (Branch (Branch Empty 'j' Empty) 'e' (Branch Empty 'k' Empty))) 'a' (Branch (Branch (Branch Empty 'l' Empty) 'f' (Branch Empty 'm' Empty)) 'c' (Branch (Branch Empty 'n' Empty) 'g' (Branch Empty 'o' Empty)))
-- "abdhiejkcflmgno"
-- 
-- >>> depthFirstBst $ Branch (Branch (Branch (Branch (Branch Empty 16 Empty) 8 (Branch Empty 17 Empty)) 4 (Branch (Branch Empty 18 Empty) 9 (Branch Empty 19 Empty))) 2 (Branch (Branch (Branch Empty 20 Empty) 10 (Branch Empty 21 Empty)) 5 (Branch (Branch Empty 22 Empty) 11 (Branch Empty 23 Empty)))) 1 (Branch (Branch (Branch (Branch Empty 24 Empty) 12 (Branch Empty 25 Empty)) 6 (Branch (Branch Empty 26 Empty) 13 (Branch Empty 27 Empty))) 3 (Branch (Branch (Branch Empty 28 Empty) 14 (Branch Empty 29 Empty)) 7 (Branch (Branch Empty 30 Empty) 15 (Branch Empty 31 Empty))))
-- [1,2,4,8,16,17,9,18,19,5,10,20,21,11,22,23,3,6,12,24,25,13,26,27,7,14,28,29,15,30,31]
-- 
-- >>> depthFirstBst $ Branch (Branch (Branch (Branch (Branch Empty 16 Empty) 8 (Branch Empty 17 Empty)) 4 (Branch (Branch Empty 18 Empty) 9 (Branch Empty 19 Empty))) 2 (Branch (Branch (Branch Empty 20 Empty) 10 (Branch Empty 21 Empty)) 5 (Branch (Branch Empty 22 Empty) 11 (Branch Empty 23 Empty)))) 1 (Branch (Branch (Branch (Branch Empty 24 Empty) 12 Empty) 6 Empty) 3 Empty)
-- [1,2,4,8,16,17,9,18,19,5,10,20,21,11,22,23,3,6,12,24]
-- 
-- >>> depthFirstBst  $ Branch (Branch (Branch (Branch (Branch Empty 16 Empty) 8 (Branch Empty 17 Empty)) 4 (Branch (Branch Empty 18 Empty) 9 (Branch Empty 19 Empty))) 2 (Branch (Branch (Branch Empty 20 Empty) 10 (Branch Empty 21 Empty)) 5 (Branch (Branch Empty 22 Empty) 11 (Branch Empty 23 Empty)))) 1 (Branch Empty 3 (Branch (Branch (Branch Empty 28 Empty) 14 (Branch Empty 29 Empty)) 7 (Branch (Branch Empty 30 Empty) 15 Empty)))
-- [1,2,4,8,16,17,9,18,19,5,10,20,21,11,22,23,3,7,14,28,29,15,30]
depthFirstBst :: Tree a -> [a]
depthFirstBst = uncurry (++) . recur
  where recur = foldTree (\(l1, r1) x (l2, r2) -> (x : l1, r1 ++ l2 ++ r2)) ([], [])

-- |
-- e.g.
--     a
--    / \
--   b   c
--  /   / \
-- d   e   f
-- 
-- should be a,b,c,d,e,f
-- 
-- >>> breadthFirstBst $ Branch ((Branch (Branch Empty 'd' Empty) 'b' Empty)) 'a' (Branch (Branch Empty 'e' Empty) 'c' (Branch Empty 'f' Empty))
-- "abcdef"
-- 
-- >>> breadthFirstBst $ Branch (Branch (Branch (Branch Empty 'h' Empty) 'd' (Branch Empty 'i' Empty)) 'b' (Branch (Branch Empty 'j' Empty) 'e' (Branch Empty 'k' Empty))) 'a' (Branch (Branch (Branch Empty 'l' Empty) 'f' (Branch Empty 'm' Empty)) 'c' (Branch (Branch Empty 'n' Empty) 'g' (Branch Empty 'o' Empty)))
-- "abcdefghijklmno"
-- 
-- >>> breadthFirstBst $ Branch (Branch (Branch (Branch (Branch Empty 16 Empty) 8 (Branch Empty 17 Empty)) 4 (Branch (Branch Empty 18 Empty) 9 (Branch Empty 19 Empty))) 2 (Branch (Branch (Branch Empty 20 Empty) 10 (Branch Empty 21 Empty)) 5 (Branch (Branch Empty 22 Empty) 11 (Branch Empty 23 Empty)))) 1 (Branch (Branch (Branch (Branch Empty 24 Empty) 12 (Branch Empty 25 Empty)) 6 (Branch (Branch Empty 26 Empty) 13 (Branch Empty 27 Empty))) 3 (Branch (Branch (Branch Empty 28 Empty) 14 (Branch Empty 29 Empty)) 7 (Branch (Branch Empty 30 Empty) 15 (Branch Empty 31 Empty))))
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
-- 
-- >>> breadthFirstBst $ Branch (Branch (Branch (Branch (Branch Empty 16 Empty) 8 (Branch Empty 17 Empty)) 4 (Branch (Branch Empty 18 Empty) 9 (Branch Empty 19 Empty))) 2 (Branch (Branch (Branch Empty 20 Empty) 10 (Branch Empty 21 Empty)) 5 (Branch (Branch Empty 22 Empty) 11 (Branch Empty 23 Empty)))) 1 (Branch (Branch (Branch (Branch Empty 24 Empty) 12 Empty) 6 Empty) 3 Empty)
-- [1,2,3,4,5,6,8,9,10,11,12,16,17,18,19,20,21,22,23,24]
-- 
-- >>> breadthFirstBst $ Branch (Branch (Branch (Branch (Branch Empty 16 Empty) 8 (Branch Empty 17 Empty)) 4 (Branch (Branch Empty 18 Empty) 9 (Branch Empty 19 Empty))) 2 (Branch (Branch (Branch Empty 20 Empty) 10 (Branch Empty 21 Empty)) 5 (Branch (Branch Empty 22 Empty) 11 (Branch Empty 23 Empty)))) 1 (Branch Empty 3 (Branch (Branch (Branch Empty 28 Empty) 14 (Branch Empty 29 Empty)) 7 (Branch (Branch Empty 30 Empty) 15 Empty)))
-- [1,2,3,4,5,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,28,29,30]
breadthFirstBst :: Tree a -> [a]
breadthFirstBst = (\(x, lists) -> maybeToList x ++ join lists) . foldTree f (Nothing, [])
  where f (lvl1, tl1) x (lvl2, tl2) = let nextLvls = catMaybes [lvl1, lvl2] in
                                      (if null nextLvls then id else (nextLvls:)) <$> (Just x, merge tl1 tl2)

        merge xs1 xs2 = let trail = if length xs1 > length xs2 then drop (length xs2) xs1 else drop (length xs1) xs2 in
                        zipWith (++) xs1 xs2 ++ trail

-- |
-- >>> isFullBst $ Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 Empty)
-- True
-- 
-- >>> isFullBst $ Branch (Branch (Branch Empty 4 Empty) 2 Empty) 1 (Branch Empty 3 Empty)
-- False
-- 
-- >>> isFullBst $ Empty
-- True
-- 
isFullBst :: Tree a -> Bool
isFullBst (Branch Empty _ Empty) = True
isFullBst (Branch Empty _ _) = False
isFullBst (Branch _ _ Empty) = False
isFullBst (Branch l _ r) = isFullBst l && isFullBst r
isFullBst _ = True

-- |
-- >>> bstBottomView $ Branch (Branch (Branch Empty 5 Empty) 8 (Branch (Branch Empty 10 Empty) 3 (Branch Empty 14 Empty))) 20 (Branch Empty 22 (Branch Empty 25 Empty))
-- [5,10,3,14,25]
-- 
-- >>> bstBottomView $ Branch (Branch (Branch Empty 5 Empty) 8 (Branch (Branch Empty 10 Empty) 3 (Branch Empty 14 Empty))) 20 (Branch (Branch Empty 4 Empty) 22 (Branch Empty 25 Empty))
-- [5,10,4,14,25]
-- 
-- >>> bstBottomView $ Branch (Branch Empty 3 Empty) 1 (Branch Empty 2 Empty)
-- [3,1,2]
-- 
-- >>> bstBottomView $ Branch (Branch (Branch Empty 40 Empty) 20 (Branch Empty 60 Empty)) 10 (Branch Empty 30 Empty)
-- [40,20,60,30]
-- 
-- >>> bstBottomView $ Branch (Branch (Branch Empty 4 (Branch (Branch (Branch Empty 10 Empty) 9 Empty) 7 Empty)) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch Empty 8 (Branch Empty 11 Empty)) 6 Empty))
-- [10,9,7,5,8,11]
-- 
-- >>> bstBottomView $ Branch (Branch (Branch Empty 4 (Branch (Branch (Branch Empty 10 Empty) 9 Empty) 7 Empty)) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch (Branch Empty 12 Empty) 8 (Branch Empty 11 Empty)) 6 Empty))
-- [10,9,7,12,8,11]
bstBottomView :: Tree a -> [a]
bstBottomView = bstProjectView (<)

-- |
-- >>> bstTopView $ Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch (Branch Empty 6 Empty) 3 (Branch Empty 7 Empty))
-- [4,2,1,3,7]
-- 
-- >>> bstTopView $ Branch (Branch Empty 2 (Branch Empty 4 (Branch Empty 5 (Branch Empty 6 Empty)))) 1 (Branch Empty 3 Empty)
-- [2,1,3,6]
bstTopView :: Tree a -> [a]
bstTopView = bstProjectView (>)

bstProjectView :: (Int -> Int -> Bool) -> Tree a -> [a]
bstProjectView compare = map value . M.elems . recur 0 0 M.empty
  where recur _ _ m Empty = m
        recur x y m (Branch l n r) = 
          let v = BstProjection n y 
              newV = maybe v (\oldV -> if compare (depth oldV) (depth v) then oldV else v) $ M.lookup x m
              mFromV = M.insert x newV m
              mFromLeft = recur (x - 1) (y - 1) mFromV l in
          recur (x + 1) (y - 1) mFromLeft r
                          
data BstProjection a = BstProjection {
  value :: a
, depth :: Int
}

-- |
-- >>> bstRightView $ Branch (Branch (Branch Empty 4 (Branch Empty 8 Empty)) 2 (Branch Empty 5 Empty)) 1 (Branch (Branch Empty 6 Empty) 3 (Branch Empty 7 Empty))
-- [1,3,7,8]
-- 
-- >>> bstRightView $ Branch (Branch Empty 3 Empty) 1 (Branch Empty 2 Empty)
-- [1,2]
-- 
-- >>> bstRightView $ Branch (Branch (Branch Empty 40 Empty) 20 (Branch Empty 60 Empty)) 10 (Branch Empty 30 Empty)
-- [10,30,60]
bstRightView :: Tree a -> [a]
bstRightView = map fst . M.elems . recur 0 0 M.empty
  where recur _ _ m Empty = m
        recur x y m (Branch l n r) = 
          let v = (n, x)
              newV = maybe v (\oldV -> if snd v > x then oldV else v) $ M.lookup y m
              mFromCurrNode = M.insert y newV m
              mFromLeft = recur (x - 1) (y + 1) mFromCurrNode l in
          recur (x + 1) (y + 1) mFromLeft r 

-- |
-- 
-- >>> isSubtree ( Branch (Branch (Branch Empty 'c' Empty) 'b' Empty) 'a' (Branch Empty 'd' (Branch Empty 'e' Empty)) ) $ Branch (Branch (Branch Empty 'c' Empty) 'b' Empty) 'a' (Branch Empty 'd' Empty)
-- False
-- 
-- >>> isSubtree ( Branch (Branch (Branch Empty 'c' Empty) 'b' Empty) 'a' (Branch Empty 'd' (Branch Empty 'e' Empty)) ) $ Branch Empty 'b' Empty
-- False
-- 
-- >>> isSubtree ( Branch (Branch (Branch Empty 'c' Empty) 'b' Empty) 'a' (Branch Empty 'd' (Branch Empty 'e' Empty)) ) $ Branch Empty 'c' Empty
-- True
-- 
-- >>> isSubtree ( Branch (Branch (Branch Empty 'c' Empty) 'b' Empty) 'a' (Branch Empty 'd' (Branch Empty 'e' Empty)) ) $ Branch Empty 'e' Empty
-- True
-- 
-- >>> isSubtree ( Branch (Branch (Branch Empty 'a' (Branch Empty 'c' Empty)) 'x' (Branch Empty 'b' Empty)) 'z' (Branch Empty 'e' (Branch Empty 'k' Empty)) ) $ Branch (Branch Empty 'a' (Branch Empty 'c' Empty)) 'x' (Branch Empty 'b' Empty)
-- True
-- 
-- >>> isSubtree ( Branch (Branch Empty 'a' (Branch Empty 'c' Empty)) 'x' (Branch Empty 'b' Empty) ) $ Branch (Branch (Branch Empty 'a' (Branch Empty 'c' Empty)) 'x' (Branch Empty 'b' Empty)) 'z' (Branch Empty 'e' (Branch Empty 'k' Empty))
-- False
-- 
-- >>> isSubtree ( Branch (Branch (Branch Empty 4 Empty) 10 (Branch (Branch Empty 30 Empty) 6 Empty)) 26 (Branch Empty 3 (Branch Empty 3 Empty)) ) $ Branch (Branch Empty 4 Empty) 10 (Branch (Branch Empty 30 Empty) 6 Empty)
-- True
-- 
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree _ Empty = True
isSubtree Empty _ = False
isSubtree main@(Branch l _ r) sub = or [main == sub, isSubtree l sub, isSubtree r sub]

-- |
-- >>> lca ( Branch (Branch (Branch Empty 4 Empty) 8 (Branch (Branch Empty 10 Empty) 12 (Branch Empty 14 Empty))) 20 (Branch Empty 22 Empty) ) 10 14
-- Just 12
-- 
-- >>> lca ( Branch (Branch (Branch Empty 4 Empty) 8 (Branch (Branch Empty 10 Empty) 12 (Branch Empty 14 Empty))) 20 (Branch Empty 22 Empty) ) 14 10
-- Just 12
-- 
-- >>> lca ( Branch (Branch (Branch Empty 4 Empty) 8 (Branch (Branch Empty 10 Empty) 12 (Branch Empty 14 Empty))) 20 (Branch Empty 22 Empty) ) 14 8
-- Just 8
-- 
-- >>> lca ( Branch (Branch (Branch Empty 4 Empty) 8 (Branch (Branch Empty 10 Empty) 12 (Branch Empty 14 Empty))) 20 (Branch Empty 22 Empty) ) 22 10
-- Just 20
-- 
-- >>> lca ( Branch (Branch (Branch Empty 3 Empty) 4 Empty) 5 (Branch Empty 6 (Branch Empty 7 (Branch Empty 8 Empty))) ) 7 8
-- Just 7
-- 
-- >>> lca ( Branch (Branch (Branch Empty 3 Empty) 4 Empty) 5 (Branch Empty 6 (Branch Empty 7 (Branch Empty 8 Empty))) ) 8 7
-- Just 7
-- 
-- >>> lca ( Branch (Branch (Branch Empty 3 Empty) 4 Empty) 5 (Branch Empty 6 (Branch Empty 7 (Branch Empty 8 Empty))) ) 8 9
-- Nothing
-- 
lca :: Tree Int -> Int -> Int -> Maybe Int
lca tree x y = do 
      xs <- trace tree x
      ys <- trace tree y
      firstCommonItem xs ys
    where trace Empty _ = Nothing
          trace (Branch l n r) x = let justN = Just [n] in 
                                   if n == x then justN
                                   else if n < x then (++) <$> trace r x <*> justN
                                   else (++) <$> trace l x <*> justN

          firstCommonItem xs@(x : xTl) ys@(y : yTl)
            | x == y = Just x
            | any (== x) ys = Just x
            | any (== y) xs = Just y
            | otherwise = firstCommonItem xTl yTl
          firstCommonItem _ _ = Nothing

-- |
-- >>> removeNodeBelowDepth ( Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch Empty 8 Empty) 6 Empty)) ) 4
-- Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 Empty) 1 (Branch Empty 3 (Branch (Branch Empty 8 Empty) 6 Empty))
-- 
-- >>> removeNodeBelowDepth ( Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch Empty 8 Empty) 6 Empty)) ) 5
-- Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 Empty) 1 Empty
-- 
-- >>> removeNodeBelowDepth ( Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch Empty 8 Empty) 6 Empty)) ) 3
-- Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch Empty 8 Empty) 6 Empty))
-- 
-- >>> removeNodeBelowDepth ( Branch (Branch (Branch (Branch (Branch Empty 9 Empty) 7 Empty) 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 (Branch (Branch Empty 8 Empty) 6 Empty)) ) 10
-- Empty
-- 
removeNodeBelowDepth :: Tree a -> Int -> Tree a
removeNodeBelowDepth tree target = recur tree 1
    where recur Empty _ = Empty
          recur node@(Branch Empty _ Empty) depth = if depth < target then Empty else node
          recur (Branch l x r) depth = case flip recur (depth + 1) <$> [l, r] of
                                            [Empty, Empty] -> Empty
                                            [newL, newR] -> Branch newL x newR

-- |
-- >>> mirrorTree $ Branch (Branch Empty 3 Empty) 1 (Branch (Branch Empty 5 Empty) 2 (Branch Empty 4 Empty))
-- Branch (Branch (Branch Empty 4 Empty) 2 (Branch Empty 5 Empty)) 1 (Branch Empty 3 Empty)
mirrorTree :: Tree a -> Tree a
mirrorTree Empty = Empty
mirrorTree (Branch l x r) = Branch (mirrorTree r) x (mirrorTree l)

-- |
-- >>> alternateReverse $ Branch (Branch (Branch (Branch Empty 'h' Empty) 'd' (Branch Empty 'i' Empty)) 'b' (Branch (Branch Empty 'j' Empty) 'e' (Branch Empty 'k' Empty))) 'a' (Branch (Branch (Branch Empty 'l' Empty) 'f' (Branch Empty 'm' Empty)) 'c' (Branch (Branch Empty 'n' Empty) 'g' (Branch Empty 'o' Empty)))
-- Just (Branch (Branch (Branch (Branch Empty 'o' Empty) 'd' (Branch Empty 'n' Empty)) 'c' (Branch (Branch Empty 'm' Empty) 'e' (Branch Empty 'l' Empty))) 'a' (Branch (Branch (Branch Empty 'k' Empty) 'f' (Branch Empty 'j' Empty)) 'b' (Branch (Branch Empty 'i' Empty) 'g' (Branch Empty 'h' Empty))))
-- 
alternateReverse :: Tree a -> Maybe (Tree a)
alternateReverse tree = recur False tree tree
    where recur :: Bool -> Tree a -> Tree a -> Maybe (Tree a) 
          recur _ Empty Empty = Just Empty
          recur swap (Branch l1 x1 r1) (Branch l2 x2 r2) = -- 1st tree is reversed, 2nd tree is original
            let toggle = not swap
                node = if swap then x1 else x2 in
            Branch <$> recur toggle r1 l2 <*> pure node <*> recur toggle l1 r2
          recur _ _ _ = Nothing

-- | 
-- the approach is a bit similar to foldl
foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree _ b Empty = b
foldlTree f b (Branch lt x rt) = foldlTree f (f (foldlTree f b lt) x) rt
          
-- | 
-- notice the difference on first argument on foldTree
-- unlike foldTree, foldrTree cannot cope with left most
-- the approach is a bit similar to foldr
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ b Empty = b
foldrTree f b (Branch lt x rt) = foldrTree f (f x (foldrTree f b rt)) lt

foldrTree_rightMost :: Tree a -> Maybe a
foldrTree_rightMost = foldrTree (\x b -> mplus b $ Just x) Nothing

foldlTree_leftMost :: Tree a -> Maybe a
foldlTree_leftMost = foldlTree (\b x -> mplus b $ Just x) Nothing

-- | noOfTrees for 0 consecutive number = 1, because there is no node, therefore only 1 tree, an empty tree.
-- noOfTrees for 1 consecutive number = 1
-- 1
-- noOfTrees for 2 consecutive numbers = 2
-- 1      2
--  \    /
--   2  1
-- noOfTrees for 3 consecutive numbers = 5
-- 1          1      2      3          3
--  \          \    / \    /          /
--   2          3  1   3  1          2
--    \        /           \        /
--     3      2             2      1
-- and so on.
-- 
-- >>> noOfBsts 0
-- 1
-- 
-- >>> noOfBsts 1
-- 1
-- 
-- >>> noOfBsts 2
-- 2
-- 
-- >>> noOfBsts 3
-- 5
-- 
-- >>> noOfBsts 4
-- 14
-- 
-- >>> noOfBsts 5
-- 42
-- 
-- >>> noOfBsts 6
-- 132
-- 
-- >>> noOfBsts 7
-- 429
-- 
-- >>> noOfBsts 8
-- 1430
-- 
-- >>> noOfBsts 9
-- 4862
noOfBsts :: Int -> Integer
noOfBsts n = bstV n V.! n
    where bstV 0 = V.fromList [1]
          bstV 1 = V.fromList [1, 1]
          bstV n = let v = bstV $ n - 1
                       evenNumVal = 2 * sum (map (\x -> v V.! x * v V.! (n - 1 - x)) [n-1, n-2 .. (n - 1) `div` 2 + 1])
                       finalVal = if n `mod` 2 == 0 
                                  then evenNumVal 
                                  else evenNumVal + square (v V.! ((n - 1) `div` 2)) in
                    v V.++ V.fromList [finalVal]
          square n = n * n

