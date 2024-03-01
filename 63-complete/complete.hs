import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
        deriving (Show, Eq)

filled :: Tree a -> [[Bool]]
filled Empty = repeat [False]
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generateTree 1
  where generateTree x
          | x > n     = Empty
          | otherwise = Branch 'x' (generateTree (2*x)) (generateTree (2*x+1))