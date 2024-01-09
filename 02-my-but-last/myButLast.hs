myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
myButLast _ = error("The list must contain at least 2 elements.")