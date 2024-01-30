import System.Random
import Control.Monad

rnd_select :: [a] -> Int -> IO [a]
rnd_select _ 0 = return []
rnd_select [] _ = return []
rnd_select xs n
    | n < 0 = error "The number of elements must not be negative."
    | otherwise = do
        gen <- getStdGen
        return $ take n [xs !! i | i <- randomRs (0, length xs - 1) gen]