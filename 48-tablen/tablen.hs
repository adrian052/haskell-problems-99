import Control.Monad (replicateM)

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = [ x ++ [f x]| x <- combinations n]

combinations :: Int -> [[Bool]] 
combinations n = replicateM n [False, True]

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
infixl 5 `and'`

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True
infixl 4 `or'`

equ' :: Bool -> Bool -> Bool
equ' = (==)
infixl 7 `equ'`

