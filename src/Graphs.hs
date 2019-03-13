{-# LANGUAGE NoImplicitPrelude
           , RecordWildCards #-}

module Graphs where

import Protolude
import Data.List (nub)
import qualified Data.Map as M

data RoseTree a = Nde a [RoseTree a] deriving Show -- is it n-ary tree can be presented by cofree

foldRoseTree :: (a -> [b] -> b) -> b -> RoseTree a -> b
foldRoseTree f y (Nde x branches) = f x $ map (foldRoseTree f y) branches

data Free f a = Pure a
              | Free f (Free f a)

data Cofree f a =  a :< f (Cofree f a)

foldCofree :: Functor f => (a -> f b -> b) -> b -> Cofree f a -> b
foldCofree g b (x :< fOfCofree) = g x (foldCofree g b <$> fOfCofree)

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

type Edge a = (a, Int) -- (vertex, cost)

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