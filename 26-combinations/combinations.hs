combinations :: Int -> [a] -> [[a]] 
combinations n arr 
    |n>=0 = combinationsUtil n arr []
    |otherwise = error("Please insert a number greather than -1")

combinationsUtil :: Int -> [a] -> [a] -> [[a]]
combinationsUtil 0 _ curr = [curr]
combinationsUtil _ [] _ = []
combinationsUtil n all@(x:xs) curr = combinationsUtil (n-1) xs (curr++[x]) ++ combinationsUtil (n) xs curr 