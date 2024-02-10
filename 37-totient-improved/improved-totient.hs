import Data.Map (fromListWith, toList)

minPrimeFactor :: Int -> Int
minPrimeFactor n = case [x | x <- [2..isqrt n], n `mod` x == 0] of
                    [] -> n
                    (x:xs) -> x
    where isqrt = floor . sqrt . fromIntegral

factorsUtil :: Int -> [Int] -> [Int]
factorsUtil n curr
    | n == 1 = curr
    | n > 1 = factorsUtil (div n minFactor) (curr ++ [minFactor])
    | otherwise = error "Negative numbers don't have prime factors"
    where minFactor = minPrimeFactor n

factors :: Int -> [(Int,Int)]
factors n
    | n <= 0 = error "Negative numbers don't have prime factors"
    | otherwise = toList (fromListWith (+) [(x, 1) | x <- (factorsUtil n [])])
        
totient :: Int -> Int
totient n = foldl (\acc (p, m) -> acc * (p-1) * p^(m-1)) 1 (factors n)