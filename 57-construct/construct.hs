data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch a t1 t2) = Branch a t2 t1

sameShape :: Tree a -> Tree a -> Bool
sameShape Empty Empty                        = True
sameShape (Branch _ t1 t2 ) (Branch _ t3 t4) = sameShape t1 t3 && sameShape t2 t3
sameShape _ _                                = False  

symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
symmetric (Branch a t1 t2) = mirror t1 `sameShape` t2

add :: Ord a => Tree a -> a -> Tree a
add Empty new = Branch new Empty Empty
add (Branch elem l r) new = if new >= elem 
                            then Branch elem l (add r new)
                            else Branch elem (add l new) r

construct :: Ord a => [a] -> Tree a
construct = foldl add Empty