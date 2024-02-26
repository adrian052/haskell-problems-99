data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Eq a => Tree a -> Int 
countLeaves Empty = 0
countLeaves (Branch _ l r) = if l == Empty && r == Empty
                                then 1
                                else countLeaves l + countLeaves r