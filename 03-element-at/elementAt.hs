elementAt :: (Num i, Ord i) => [a] -> i -> a
elementAt [] _ = error "The element was not found."
elementAt (x:_) 0 = x
elementAt (_:xs) n
    | n < 0     = error "La index must not be negative."
    | otherwise = elementAt xs (n - 1)