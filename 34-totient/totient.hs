myGCD :: Int -> Int -> Int 
myGCD a b 
    | b == 0 = a
    | otherwise = myGCD b (mod a b)

coprimes :: Int -> Int -> Bool
coprimes a b = (myGCD a b) == 1

totient :: Int -> Int
totient 1 = 1
totient n = foldl (\acc x -> if (coprimes x n) then acc+1 else acc) 0 [1..n-1]