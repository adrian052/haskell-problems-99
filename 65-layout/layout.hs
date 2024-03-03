data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )


height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = 1 + max (height l) (height r)  

minSecond :: Tree (a, Int) -> Int
minSecond Empty = 1000000000000000
minSecond (Branch (_,s) l r) = min (s) (min (minSecond r) (minSecond l))

labelTree :: Tree a -> Int -> Int -> Tree (a, Int)
labelTree Empty _ _= Empty
labelTree (Branch x l r) last h = Branch (x, last) (labelTree l (last-(2^h)) (h-1)) (labelTree r (last+(2^h)) (h-1))

sumSecond :: Int -> Tree (a, Int) -> Tree (a, Int)
sumSecond _ Empty = Empty 
sumSecond n (Branch (x,s) l r) = (Branch (x,s+n) (sumSecond n l) (sumSecond n r))

addHeight :: Tree (a,Int) -> Int -> Tree (a,(Int,Int))
addHeight (Empty) _ = Empty
addHeight (Branch (f,s) l r) h = Branch (f,(s,h)) (addHeight l (h+1)) (addHeight r (h+1))

layout :: Tree a -> Tree (a,(Int, Int))
layout tree = addHeight (sumSecond ((abs (minSecond labeled))+1) labeled) 1
            where labeled = (labelTree tree 1 (height tree-2))