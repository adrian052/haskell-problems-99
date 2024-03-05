import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree66 = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

stringToTree :: String -> Tree Char 
stringToTree [] = Empty
stringToTree (x:[]) = Branch x Empty Empty
stringToTree str    = Branch (str !! 0) (stringToTree leftChild) (stringToTree rightChild)
                where childs = substring 2 ((length str)-1) str 
                      pivot = commaNotEncolsed childs
                      leftChild = substring 0 (pivot) childs
                      rightChild = substring (pivot+1) (length childs) childs 

substring :: Int -> Int -> String -> String
substring i j s = take (j-i) ( drop i s )

commaNotEncolsed :: String -> Int
commaNotEncolsed str = go 0 0 False str
  where
    go :: Int -> Int -> Bool -> String -> Int
    go _ _ _ [] = -1
    go idxCount idx inParentheses (x:xs)
      | x == '(' = go (idxCount + 1) (idx + 1) True xs
      | x == ')' = go (idxCount + 1) (idx + 1) False xs
      | x == ',' && not inParentheses = idxCount
      | otherwise = go (idxCount + 1) (idx + 1) inParentheses xs