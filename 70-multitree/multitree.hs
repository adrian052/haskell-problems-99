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

nnodes :: Tree a -> Int
nnodes (Node a []) = 1
nnodes (Node a xs) = foldl (\acc x -> acc+(nnodes x)) 1 xs 

treeToString :: Tree Char -> String
treeToString (Node a []) = [a] ++ "^"
treeToString (Node a children) = [a] ++ concatMap treeToString children ++ "^"