isPrime :: Int -> Bool
isPrime n = foldl (\acc x -> if ((mod n x) == 0) then False else acc&&True) True [2..n-1]