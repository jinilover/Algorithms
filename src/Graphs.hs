{-# LANGUAGE RecordWildCards #-}
module Graphs where

import Protolude
import Data.List
import qualified Data.Map as M

-- data RoseTree a = Node a [RoseTree a] deriving Show -- is it n-ary tree can be presented by cofree

-- foldRoseTree :: (a -> [b] -> b) -> b -> RoseTree a -> b
-- foldRoseTree f y (Node x branches) = f x $ map (foldRoseTree f y) branches

data Free f a = Pure a
              | Free f (Free f a)

data Cofree f a =  a :< f (Cofree f a)

foldCofree :: Functor f => (a -> f b -> b) -> b -> Cofree f a -> b
foldCofree g b (x :< fOfCofree) = g x (foldCofree g b <$> fOfCofree)

data Node a = Node {
    value :: a
  , adjacents :: [a]
} deriving Show

type Graph a = [Node a]

generate :: Eq a => [a] -> Graph a
generate (x1 : x2 : xs) = addNode x2 x1 $ addNode x1 x2 $ generate xs
  where addNode :: Eq a => a -> a -> Graph a -> Graph a
        addNode x1 x2 xs =
          let newNode = maybe (Node x1 [x2]) (\node@Node{..} ->
                        node {adjacents = x2 : adjacents}) $ find ((x1 ==) . value) xs in
          newNode : filter ((x1 /=) . value) xs
generate _ = []

-- |
-- >>> bfsGraph 1 5 $ generate [1,2,1,3,1,4,3,5]
-- [1,2,3,4,5]
-- 
-- >>> bfsGraph 3 5 $ generate [1,2,1,3,1,4,3,5]
-- [3,1,5]
-- 
-- >>> bfsGraph 3 6 $ generate [1,2,1,3,1,4,3,5]
-- []
-- 
-- >>> bfsGraph 0 5 $ generate [1,2,1,3,1,4,3,5]
-- []
-- 
-- >>> bfsGraph 1 9 $ generate [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,3,4,5,6,7,8,9]
-- 
-- >>> bfsGraph 1 12 $ generate [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,3,4,5,6,7,8,9,10,11,12]
-- 
-- >>> bfsGraph 1 5 $ generate [1,2,1,3,1,4,2,5,2,6,2,7,3,8,3,9,3,10,4,11,4,12]
-- [1,2,3,4,5]
bfsGraph :: Eq a => a -> a -> Graph a -> [a]
bfsGraph src dest graph = maybe [] (\node -> 
    let (path, found, _) = traverseNodes [] [node] [] in if found then path else []
  ) $ findNode graph src
  where traverseNodes visited [] adjs = (visited, False, adjs)
        traverseNodes visited (Node{..} : nodes) adjs
            | elem value visited = traverseNodes visited nodes adjs
            | value == dest = (visited ++ [dest], True, [])
            | otherwise = let (newVisited, found, allSiblingAdjs) = traverseNodes (visited ++ [value]) nodes (adjs ++ adjacents) in
                          if found 
                            then (newVisited, True, [])
                            else traverseNodes newVisited (mapMaybe (findNode graph) . nub $ allSiblingAdjs) []

findNode :: Eq a => Graph a -> a -> Maybe (Node a)
findNode graph v = find ((== v) . value) graph