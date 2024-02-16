and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool,Bool)]
table expr = [(a,b,expr a b)| a <- [True, False], b <- [True, False]]