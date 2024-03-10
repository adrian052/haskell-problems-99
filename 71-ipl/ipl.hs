data Tree a = Node a [Tree a]
        deriving (Eq, Show)

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

ipl :: Tree a -> Int
ipl tree = ipl' 0 tree

ipl' :: Int -> Tree a -> Int
ipl' h (Node _ []) = h
ipl' h (Node _ xs) = foldl (\acc x -> acc + (ipl' (h+1) x)) h xs