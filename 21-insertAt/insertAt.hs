insertAt :: a -> [a] -> Int -> [a]
insertAt elem arr i 
    | i>=1 && i<=(length arr)+1 = [x | (x, idx) <- zip arr [1..],idx<i] ++ [elem] ++ [x | (x, idx) <- zip arr [1..],idx>=i] 
    | otherwise = error("Insert a valid index")