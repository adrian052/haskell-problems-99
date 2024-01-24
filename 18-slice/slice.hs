slice :: [a] -> Int -> Int -> [a]
slice arr i k = [x | (x,idx) <- zip arr [1..], idx>=i && idx<=k]