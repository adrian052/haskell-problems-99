split :: [a] -> Int -> ([a],[a])
split arr n = ([x | (x,i) <- zip arr [1..], i<=3],[x | (x,i) <- zip arr [1..], i>3])