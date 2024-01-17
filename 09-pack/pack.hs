pack :: (Eq a) => [a] -> [[a]]
pack arr = foldl f [] arr 
  where f acc x | null acc || last (last acc) /= x = acc ++ [[x]]
                | otherwise = init acc ++ [last acc ++ [x]]