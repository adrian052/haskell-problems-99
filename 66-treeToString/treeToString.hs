data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree66 = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

treeToString ::Tree Char -> String 
treeToString Empty = ""
treeToString (Branch n l r) 
    | l == Empty && r == Empty = [n]
    | otherwise = [n] ++ "(" ++treeToString l++","++treeToString r++")"
