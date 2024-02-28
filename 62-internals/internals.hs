data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals :: Eq a => Tree a -> [a]
internals Empty            = []
internals (Branch x l r)   = if l == Empty && r == Empty
                            then []
                            else [x] ++ internals l ++ internals r