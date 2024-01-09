myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs 
myLast _ = error ("The list must contain at least 1 element.")