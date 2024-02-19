--This code is not correct but have some useful functions to complete the problem

import Data.List

huffman :: [(Char, Int)] -> [(Char, String)]
huffman f = getHuffmanCodes (createHuffmanTree  f) [] []
data HuffmanTree = Node Int HuffmanTree HuffmanTree | Leaf Int Char deriving (Show)

createHuffmanTree :: [(Char,Int)] -> HuffmanTree
createHuffmanTree c = foldl addNode (toLeaf $ head c') (tail c')
                    where c' = sortBy sortAlg c

toLeaf :: (Char, Int) -> HuffmanTree
toLeaf elem = Leaf (snd elem) (fst elem)

addNode :: HuffmanTree -> (Char, Int) -> HuffmanTree
addNode root elem = if getWeight root > snd elem  
                    then Node  sum' (toLeaf elem) root 
                    else Node sum' root (toLeaf elem)
                    where sum' = (getWeight root + snd elem)

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
