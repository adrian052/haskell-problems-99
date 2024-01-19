data Element a = Multiple Int a | Single a deriving (Show)

decode :: [Element a] -> [a]
decode arr = foldl (\acc x -> acc ++ elemToArr x) [] arr

elemToArr :: (Element a) -> [a]
elemToArr (Multiple n x) = (replicate n x) 
elemToArr (Single x) = [x]
