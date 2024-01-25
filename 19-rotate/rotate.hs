rotate :: [a] -> Int -> [a]
rotate arr n 
    | n > 0 = [x | (idx, x) <- zip [0..] arr, idx >= (mod n (length arr)) && idx < length arr] ++ [x | (idx, x) <- zip [0..] arr, idx >= 0 && idx < (mod n (length arr))] 
    | n < 0 = [x | (idx, x) <- zip [0..] arr, idx >= (length arr + n) && idx < length arr] ++ [x | (idx, x) <- zip [0..] arr, idx >= 0 && idx < (length arr + n)]
    | otherwise = arr
