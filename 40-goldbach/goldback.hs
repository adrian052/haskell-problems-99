goldbach :: Int -> (Int, Int)
goldbach n
    | n<=0 = error ("Pleas insert positive numbers.")
    | even n = goldbach' 1 n
    | otherwise = error ("Goldback conjetures only applies to even numbers.")

goldbach' :: Int -> Int -> (Int, Int)
goldbach' curr n 
    | isPrime curr && isPrime (n-curr) = (curr, n-curr)
    | otherwise = goldbach' (curr+1) n

isPrime :: Int -> Bool
isPrime n = foldl (\acc x -> if ((mod n x) == 0) then False else acc&&True) True [2..n-1]