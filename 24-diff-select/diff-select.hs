import System.Random
import Data.List (nub)

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
  gen <- getStdGen
  return $ take n $ nub $ randomRs (1, m) gen

