table :: (Bool -> Bool -> Bool) -> [(Bool, Bool,Bool)]
table expr = [(a,b,expr a b)| a <- [True, False], b <- [True, False]]