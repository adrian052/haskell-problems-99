myReverse :: [a] -> [a]
myReverse (x:xs) = (myReverse xs) ++ [x] 
myReverse [] = []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (myReverse list) == list