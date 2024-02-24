data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]
hbalTree elem n = filter (\x -> height x==n && isHb x ==True) (buildTrees elem n)  

buildTrees :: a -> Int -> [Tree a]
buildTrees elem 0 = []
buildTrees elem n = [Branch elem x y | x <- [Empty]++buildTrees elem (n-1) 
                                     , y <- [Empty]++buildTrees elem (n-1)]

height :: Tree a -> Int
height Empty = 0 
height (Branch a l r) = 1 + max (height l) (height r)

isHb :: Tree a -> Bool
isHb Empty = True 
isHb (Branch a l r) = abs(height l - height r)<=1 && isHb l && isHb r

