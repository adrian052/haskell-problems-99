data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

buildTrees :: Int -> a -> [Tree a]
buildTrees n x
    | n <= 0 = [Empty]
    | otherwise = [Branch x leftSubtree rightSubtree | leftNodes <- [0..(n-1)],
                                                     let rightNodes = n - 1 - leftNodes,
                                                     leftSubtree <- buildTrees leftNodes x,
                                                     rightSubtree <- buildTrees rightNodes x]

isHb :: Tree a -> Bool
isHb Empty = True 
isHb (Branch a l r) = abs(height l - height r)<=1 && isHb l && isHb r

height :: Tree a -> Int
height Empty = 0 
height (Branch a l r) = 1 + max (height l) (height r)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = filter isHb (buildTrees n x)