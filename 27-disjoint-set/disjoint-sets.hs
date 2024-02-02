import Data.List

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g:gs | (g,rest) <- combinations n xs, gs <- Main.group ns rest]

combinations :: Int -> [a] -> [([a],[a])]
combinations 0 xs = [([], xs)]
combinations n [] = []
combinations n (x:xs) = [(x:ys, zs) | (ys, zs) <- combinations (n-1) xs] ++ combinations n xs
