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
