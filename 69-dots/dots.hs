data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree1 = (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))

tree2ds :: Tree Char -> String 
tree2ds Empty           = "."
tree2ds (Branch x l r)  = [x] ++ tree2ds l ++ tree2ds r 

ds2tree :: String -> Tree Char
ds2tree str = fst $ buildTree str

buildTree :: String -> (Tree Char, String)
buildTree ('.':xs) = (Empty, xs)
buildTree (x:xs) = (Branch x left right, restRight)
    where (left, restLeft) = buildTree xs
          (right, restRight) = buildTree restLeft