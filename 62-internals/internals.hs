data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals :: Eq a => Tree a -> [a]
internals Empty            = []
internals (Branch x l r)   = if l == Empty && r == Empty
                            then []
                            else [x] ++ internals l ++ internals r


atLevel :: Eq a => Tree a -> Int -> [a]
atLevel tree lvl = atLevel' tree 1 lvl

atLevel':: Eq a => Tree a -> Int -> Int -> [a]
atLevel' Empty _ _                  = []
atLevel' (Branch x l r) currLvl lvl = if currLvl == lvl 
                                    then [x]
                                    else atLevel' l (currLvl+1) lvl ++  atLevel' r (currLvl+1) lvl