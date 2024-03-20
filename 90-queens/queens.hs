import Data.List (foldl')


build :: Int -> [Int] -> [[Int]]
build n lastQueens
    | length lastQueens == n = [lastQueens]
    | otherwise = concatMap (\x -> if validQueen lastQueens x 
                                    then build n (lastQueens++[x]) 
                                    else []) [1..n]


validQueen :: [Int] -> Int -> Bool
validQueen lastQueens currRow = foldl' (\acc (idx, row) -> acc && (atackSafe (row, idx) (currRow, currCol))) True (indexed lastQueens)
                                where currCol = length lastQueens + 1 


atackSafe :: (Int, Int) -> (Int, Int) -> Bool
atackSafe (row1,col1) (row2,col2) = row1 /= row2 && abs (row2 - row1) /= abs (col2 - col1)

indexed :: [Int] -> [(Int, Int)]
indexed xs = zip [1..] xs

queens :: Int -> [[Int]]
queens n = build n [] 