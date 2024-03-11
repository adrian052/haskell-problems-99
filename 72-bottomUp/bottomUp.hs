data Tree a = Node a [Tree a]
        deriving (Eq, Show)

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

bottomUp :: Tree Char -> String
bottomUp (Node n [])        = [n]
bottomUp (Node n xs)    = (foldl (\acc y -> acc++bottomUp y) [] xs)++[n]