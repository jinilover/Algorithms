module RoseTrees where

import Protolude

data RoseTree a = Nde a [RoseTree a] deriving Show -- is it n-ary tree can be presented by cofree

foldRoseTree :: (a -> [b] -> b) -> b -> RoseTree a -> b
foldRoseTree f y (Nde x branches) = f x $ map (foldRoseTree f y) branches

data Free f a = Pure a
                | Free f (Free f a)

data Cofree f a =  a :< f (Cofree f a)

foldCofree :: Functor f => (a -> f b -> b) -> b -> Cofree f a -> b
foldCofree g b (x :< fOfCofree) = g x (foldCofree g b <$> fOfCofree)

