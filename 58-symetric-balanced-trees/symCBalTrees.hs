data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
cbalTree :: Int -> [Tree Char]
cbalTree n
    | n < 0 = error "Insert a non-negative number"
    | n == 0 = [Empty]
    | otherwise = filter isBalanced (buildTrees (n))

buildTrees :: Int -> [Tree Char]
buildTrees n
    | n <= 0 = [Empty]
    | otherwise = [Branch 'x' leftSubtree rightSubtree | leftNodes <- [0..(n-1)],
                                                     let rightNodes = n - 1 - leftNodes,
                                                     leftSubtree <- buildTrees leftNodes,
                                                     rightSubtree <- buildTrees rightNodes]

isBalanced :: Tree Char -> Bool
isBalanced Empty          = True 
isBalanced (Branch _ l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r

height :: Tree a -> Int
height Empty            = 0
height (Branch _ l r)   = 1 + max (height l) (height r)

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch a t1 t2) = Branch a t2 t1

sameShape :: Tree a -> Tree a -> Bool
sameShape Empty Empty                        = True
sameShape (Branch _ t1 t2 ) (Branch _ t3 t4) = sameShape t1 t3 && sameShape t2 t4
sameShape _ _                                = False  

symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
symmetric (Branch a t1 t2) = mirror t1 `sameShape` t2

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric  $ cbalTree n  