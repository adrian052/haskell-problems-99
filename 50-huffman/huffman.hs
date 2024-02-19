import Data.List

huffman :: [(Char, Int)] -> [(Char, String)]
huffman f = sort $ getHuffmanCodes (createHuffmanTree $ sortFreqs f) [] []
data HuffmanTree = Node Int HuffmanTree HuffmanTree | Leaf Int Char deriving (Show)

createHuffmanTree :: [(Char, Int)] -> HuffmanTree
createHuffmanTree = buildTree . map (\(c, f) -> Leaf f c) . sortFreqs

sortFreqs :: [(Char, Int)] -> [(Char, Int)]
sortFreqs = sortBy sortAlg

buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [tree] = tree
buildTree (tree1:tree2:trees) = buildTree $ insertTree (mergeTrees tree1 tree2) trees
    where
        insertTree :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
        insertTree t [] = [t]
        insertTree t (x:xs)
            | getWeight t <= getWeight x = t : x : xs
            | otherwise = x : insertTree t xs

mergeTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTrees t1 t2 = Node (getWeight t1 + getWeight t2) t1 t2

getWeight :: HuffmanTree -> Int
getWeight (Node w _ _) = w 
getWeight (Leaf w _) = w


sortAlg :: (Char, Int) -> (Char, Int) -> Ordering
sortAlg (a1, b1) (a2, b2)
  | b1>b2 = GT
  | b1<b2 = LT
  | otherwise = EQ

getHuffmanCodes :: HuffmanTree -> String -> [(Char,String)] -> [(Char,String)]
getHuffmanCodes (Node _ left right) currStr currList = 
    getHuffmanCodes left (currStr ++ "0") (getHuffmanCodes right (currStr ++ "1") currList)
getHuffmanCodes (Leaf _ c) currStr currList = currList ++ [(c, currStr)]
