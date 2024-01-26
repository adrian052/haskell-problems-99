removeAt :: Int-> [a] -> (a,[a])
removeAt i arr 
    | i>=1 && i<=length arr = (arr!!(i-1), [x | (x, idx) <- zip arr [1..], idx/=i])
    | otherwise = error("Please insert a valid index")