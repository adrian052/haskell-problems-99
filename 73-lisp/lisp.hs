data Tree a = Node a [Tree a]
        deriving (Eq, Show)

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]


lisp :: Tree Char -> String
lisp (Node num [])     = [num]  
lisp (Node num xs)     = "("++[num]++(foldl (\acc x -> acc++" "++lisp x) [] xs)++")"
