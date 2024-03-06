import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree68 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

inorder :: Tree Char -> String
inorder Empty           = ""
inorder (Branch x l r)  = inorder l ++ [x] ++ inorder r

preorder :: Tree Char -> String
preorder Empty           = ""
preorder (Branch x l r)  = [x] ++ preorder l ++ preorder r   

preInTree :: String -> String -> Tree Char
preInTree _ [] = Empty
preInTree inorder preorder@(x:xs) =
    let pivot = case elemIndex x inorder of
                    Just idx -> idx
                    Nothing -> error "Invalid input: preorder does not match inorder"
        (leftPreorder, rightPreorder) = pivotSplit pivot inorder xs
    in Branch x (preInTree inorder leftPreorder) (preInTree inorder rightPreorder)

                                  

pivotSplit :: Int -> String -> String -> (String, String)
pivotSplit pivot inorder preorder =  foldl (\(l', r') x ->
                                            case elemIndex x inorder of
                                                Just idx -> if idx < pivot
                                                            then (l' ++ [x], r')
                                                            else (l', r' ++ [x])
                                                Nothing -> (l', r')
                                            ) ([], []) preorder

