{-# LANGUAGE NoImplicitPrelude
           , RecordWildCards
           , TupleSections #-}

module Graphs where

import Protolude
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Vector as V

data Node a = Node {
    node :: a
  , adjacents :: [a]
} deriving Show

type Graph a = [Node a]

genGraph :: Eq a => [a] -> Graph a
genGraph (x1 : x2 : xs) =
  addNode x2 x1 $ addNode x1 x2 $ genGraph xs
  where addNode :: Eq a => a -> a -> Graph a -> Graph a
        addNode x1 x2 xs =
          let newNode = maybe (Node x1 [x2]) (\nd ->
                        nd {adjacents = x2 : adjacents nd}) $
                        find ((x1 ==) . node) xs in
          newNode : filter ((x1 /=) . node) xs
genGraph _ = []

-- |
-- >>> bfsGraph 1 4 $ genGraph [1,2,1,3,1,4,3,5]
-- [1,2,3,4]
-- 
-- >>> bfsGraph 1 5 $ genGraph [1,2,1,3,1,4,3,5]
-- [1,2,3,4,5]
-- 
-- >>> bfsGraph 3 5 $ genGraph [1,2,1,3,1,4,3,5]
-- [3,1,5]
-- 
-- >>> bfsGraph 3 6 $ genGraph [1,2,1,3,1,4,3,5]
-- []
-- 
-- >>> bfsGraph 0 5 $ genGraph [1,2,1,3,1,4,3,5]
-- []
-- 
-- >>> bfsGraph 1 9 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,3,4,5,6,7,8,9]
-- 
-- >>> bfsGraph 1 12 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,3,4,5,6,7,8,9,10,11,12]
-- 
-- >>> bfsGraph 1 5 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,3,4,5]
bfsGraph :: Eq a => a -> a -> Graph a -> [a]
bfsGraph src dest graph = maybe [] (\node ->
    let (path, found, _) = checkNodes [] [node] [] in if found then path else []
  ) $ findNode graph src
  where checkNodes visited [] adjs = (visited, False, adjs)
        checkNodes visited (Node{..} : nodes) adjs
            | elem node visited = checkNodes visited nodes adjs
            | node == dest = (visited ++ [dest], True, [])
            | otherwise = let (newVisited, found, allSiblingAdjs) = checkNodes (visited ++ [node]) nodes (adjs ++ adjacents) in
                          if found
                            then (newVisited, True, [])
                            else checkNodes newVisited (mapMaybe (findNode graph) . nub $ allSiblingAdjs) []

-- |
-- >>> dfsGraph 1 4 $ genGraph [1,2,1,3,1,4,3,5]
-- [1,2,3,5,4]
-- 
-- >>> dfsGraph 1 5 $ genGraph [1,2,1,3,1,4,3,5]
-- [1,2,3,5]
-- 
-- >>> dfsGraph 3 5 $ genGraph [1,2,1,3,1,4,3,5]
-- [3,1,2,4,5]
-- 
-- >>> dfsGraph 3 6 $ genGraph [1,2,1,3,1,4,3,5]
-- []
-- 
-- >>> dfsGraph 0 5 $ genGraph [1,2,1,3,1,4,3,5]
-- []
-- 
-- >>> dfsGraph 1 9 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,5,6,7,3,8,9]
-- 
-- >>> dfsGraph 1 12 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,5,6,7,3,8,9,10,4,11,12]
-- 
-- >>> dfsGraph 1 5 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,5]
-- 
-- >>> dfsGraph 1 7 $ genGraph [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,5,6,7]
dfsGraph :: Eq a => a -> a -> Graph a -> [a]
dfsGraph src dest graph = maybe [] (\node ->
    let (path, found) = checkNodes [] [node] in if found then path else []
  ) $ findNode graph src
  where checkNodes visited [] = (visited, False)
        checkNodes visited (Node{..} : nodes)
          | elem node visited = checkNodes visited nodes
          | node == dest = (visited ++ [node], True)
          | otherwise = let (newVisited, found) = checkNodes (visited ++ [node]) . mapMaybe (findNode graph) $ adjacents in
                        if found
                          then (newVisited, True)
                          else checkNodes newVisited nodes

findNode :: Eq a => Graph a -> a -> Maybe (Node a)
findNode = findPoint node

type Edge a = (a, Int)

data Vertex a = Vertex {
    vertex :: a
  , edges :: [Edge a]
} deriving Show

type Web a = [Vertex a]

findVertex :: Eq a => Web a -> a -> Maybe (Vertex a)
findVertex = findPoint vertex

findPoint :: Eq a => (b -> a) -> [b] -> a -> Maybe b
findPoint f xs x = find ((x ==) . f) xs

genWeb :: Eq a => [(a, a, Int)] -> Web a
genWeb ((x1, x2, cost) : xs) = addVertex x2 x1 cost $ addVertex x1 x2 cost $ genWeb xs
  where addVertex x1 x2 cost xs =
          let newVertex = maybe (Vertex x1 [(x2, cost)]) (\v ->
                            v {edges = (x2, cost) : edges v}
                          ) $ findVertex xs x1 in
          newVertex : filter ((/= x1) . vertex) xs
genWeb [] = []

-- |
-- >>> shortestPath 1 3 $ genWeb [(1,2,10),(1,3,20),(1,4,1),(2,3,16),(3,4,4)]
-- [1,4,3]
-- 
-- >>> shortestPath 2 3 $ genWeb [(1,2,10),(1,3,20),(1,4,1),(2,3,16),(3,4,4)]
-- [2,1,4,3]
-- 
-- >>> shortestPath 'a' 'b' $ genWeb [('a', 'b', 12), ('a', 'c', 7), ('a', 'd', 5), ('b', 'c', 4), ('b', 'g', 7), ('c', 'd', 9), ('c', 'e', 4), ('c', 'g', 3), ('d', 'e', 7), ('e', 'f', 5), ('e', 'g', 2), ('f', 'g', 2)]
-- "acb"
shortestPath :: Eq a => a -> a -> Web a -> [a]
shortestPath src dest w
  | src == dest = []
  | otherwise = maybe [] (\Vertex{..} ->
        let (_, minPath) = checkVertices [] edges Nothing in
        maybe [] ((src :) . map fst) minPath
      ) $ findVertex w src
  where checkVertices visited [] minPath = (visited, minPath)
        checkVertices visited (edge@(vertex, cost) : eds) minPath
          | vertex == dest = checkVertices visited eds . takeMin minPath $ visited ++ [edge]
          | any ((== vertex) . fst) visited || vertex == src = checkVertices visited eds minPath
          | otherwise =
            maybe (checkVertices visited eds minPath) (\Vertex{..} ->
              let (_, newMin) = checkVertices (visited ++ [edge]) edges minPath in
              checkVertices visited eds newMin
            ) $ findVertex w vertex

        takeMin Nothing newPath = Just newPath
        takeMin path newPath = (\p -> if costs p < costs newPath then p else newPath) <$> path

costs :: [(a, Int)] -> Int
costs = sum . map snd
        
allPaths :: Eq a => a -> a -> Web a -> [[a]]
allPaths src dest w
  | src == dest = []
  | otherwise = maybe [] (\Vertex{..} ->
      let (_, paths) = checkVertices [] edges [] in (src:) . (fst <$>) <$> paths
    ) $ findVertex w src
  where checkVertices visited [] paths = (visited, paths)
        checkVertices visited (edge@(vertex, cost) : eds) paths
          | vertex == dest = checkVertices visited eds ((visited ++ [edge]) : paths)
          | any ((== vertex) . fst) visited || vertex == src = checkVertices visited eds paths
          | otherwise =
            maybe (checkVertices visited eds paths) (\Vertex{..} ->
              let (_, newPaths) = checkVertices (visited ++ [edge]) edges paths in
              checkVertices visited eds newPaths
            ) $ findVertex w vertex

type Path = Maybe [(Int, Int)]

type Matrix = V.Vector (V.Vector Path) -- (Index, cost)

-- |
-- >>> optimumPath 1 3 [(1,2,10),(1,3,20),(1,4,1),(2,3,16),(3,4,4)]
-- [1,4,3]
-- 
-- >>> optimumPath 2 3 [(1,2,10),(1,3,20),(1,4,1),(2,3,16),(3,4,4)]
-- [2,1,4,3]
-- 
-- >>> optimumPath 'a' 'b' [('a', 'b', 12), ('a', 'c', 7), ('a', 'd', 5), ('b', 'c', 4), ('b', 'g', 7), ('c', 'd', 9), ('c', 'e', 4), ('c', 'g', 3), ('d', 'e', 7), ('e', 'f', 5), ('e', 'g', 2), ('f', 'g', 2)]
-- "acb"
optimumPath :: Ord a => a -> a -> [(a, a, Int)] -> [a]
optimumPath src dest xs
  | src == dest = []
  | exists src && exists dest = 
      let (v, m) = buildDataStr xs
          [srcIdx, destIdx] = (`V.elemIndex` v) <$> [src, dest]
          maybePath = join $ index (floydWarshall m) <$> srcIdx <*> destIdx in
          maybe [] (\p -> src : [v V.! x| (x, _) <- p]) maybePath
  | otherwise = []
    where exists src = any (\(x1, x2, _) -> src `elem` [x1, x2]) xs

buildDataStr :: Ord a => [(a, a, Int)] -> (V.Vector a, Matrix)
buildDataStr xs = let (v, m) = recur xs V.empty M.empty in (v, buildMatrix v m)
  where recur [] v m = (v, m)
        recur ((x1, x2, cost) : xs) v m =
          let newV = updateV x1 $ updateV x2 v
              newM = M.insert (x2, x1) cost $ M.insert (x1, x2) cost m in
          recur xs newV newM

        updateV x v
          | x `elem` v = v
          | otherwise = V.cons x v

buildMatrix :: Ord a => V.Vector a -> M.Map (a, a) Int -> Matrix
buildMatrix v m = let seqNums = V.fromList [0..(length v - 1)] in
                  (\r -> buildCol r <$> seqNums) <$> seqNums
  where buildCol r c 
          | r == c = Nothing
          | otherwise = let k = (v V.! r, v V.! c) in return . (c,) <$> M.lookup k m

floydWarshall ::  Matrix -> Matrix
floydWarshall matrix = let seqNums = V.fromList [0..(length matrix - 1)] in
                       foldl (\m k -> (\r -> updateCol m k r <$> seqNums) <$> seqNums) matrix seqNums
  where updateCol m k r c 
          | k == r || k == c || r == c = index m r c
          | otherwise = let origPath = index m r c
                            rkPath = index m r k
                            kcPath = index m k c in
                        takeMin origPath $ rkPath >>= ((<$> kcPath) . (++))

        takeMin Nothing p = p
        takeMin p Nothing = p
        takeMin (Just p1) (Just p2) = if costs p1 < costs p2 then Just p1 else Just p2

index :: Matrix -> Int -> Int -> Maybe [(Int, Int)]
index m r c = (m V.! r ) V.! c