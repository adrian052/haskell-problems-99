compress :: (Eq a) => [a] -> [a]
compress arr = foldl f [] arr 
  where f acc x | null acc || last acc /= x = acc ++ [x]
                | otherwise = acc