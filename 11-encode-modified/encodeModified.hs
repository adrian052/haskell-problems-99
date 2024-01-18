data Element a = Multiple Int a | Single a deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack arr = foldl f [] arr 
  where f acc x | null acc || last (last acc) /= x = acc ++ [[x]]
                | otherwise = init acc ++ [last acc ++ [x]]

firstAndLength ::[[a]] -> [Element a]
firstAndLength arr = map  convertToElement arr


convertToElement :: [a] -> Element a
convertToElement x = if length x == 1 
                     then Single (head x) 
                     else Multiple (length x) (head x)

encode :: (Eq a) => [a] -> [Element a]
encode arr = firstAndLength (pack arr) 
