import System.Random
import Data.List

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  gen <- newStdGen
  return $ shuffle gen xs

shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle gen xs = let
    (index, newGen) = randomR (0, length xs - 1) gen
    selected = xs !! index
    remaining = take index xs ++ drop (index + 1) xs
  in
    selected : shuffle newGen remaining
