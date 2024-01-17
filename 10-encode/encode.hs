pack :: (Eq a) => [a] -> [[a]]
pack arr = foldl f [] arr 
  where f acc x | null acc || last (last acc) /= x = acc ++ [[x]]
                | otherwise = init acc ++ [last acc ++ [x]]

firstAndLength ::[[a]] -> [(Int,a)]
firstAndLength arr = map (\x -> (length x, head x)) arr

encode :: (Eq a) => [a] -> [(Int,a)]
encode arr = firstAndLength (pack arr) 