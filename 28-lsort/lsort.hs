import Data.List

compareByLength :: [a] -> [a] -> Ordering
compareByLength list1 list2
    | length list1 < length list2 = LT
    | length list1 > length list2 = GT
    | otherwise                   = EQ

lsort :: [[a]] -> [[a]] 
lsort = sortBy compareByLength 
