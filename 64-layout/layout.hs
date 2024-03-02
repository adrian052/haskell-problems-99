data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )


layout :: Tree a -> Tree (a, (Int,Int))
layout = dfsInorder 1 1

dfsInorder :: Int -> Int -> Tree a -> Tree (a, (Int,Int))
dfsInorder _ height Empty                  = Empty
dfsInorder cLabel height (Branch x l r)    = Branch (x,(cLabel+size l, height))(dfsInorder cLabel (height+1) l) (dfsInorder (cLabel+size l+1) (height+1) r)

size :: Tree a -> Int
size Empty = 0
size (Branch _ l r) = 1 + size l + size r