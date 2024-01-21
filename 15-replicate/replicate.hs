repli :: [a] -> Int -> [a]
repli arr n = foldl (\acc x -> acc ++ (replicate n x)) [] arr