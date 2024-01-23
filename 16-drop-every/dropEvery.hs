dropEvery :: [a] -> Int -> [a]
dropEvery arr n = [x | (x,i) <- zip arr [1..], (mod i n) /= 0]
