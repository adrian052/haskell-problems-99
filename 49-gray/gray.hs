import Data.Bits
import Numeric
import Data.Char

grayCode :: Int -> Int
grayCode n = n `xor` (shiftR n 1)

intToBinary :: Int -> Int -> Char -> String
intToBinary n minLength fillChar = let binStr = showIntAtBase 2 intToDigit n ""
                                   in replicate (max 0 (minLength - length binStr)) fillChar ++ binStr

gray :: Int -> [String]
gray n = foldl (\acc x -> acc ++ [intToBinary (grayCode x) n '0']) [] [x | x <- [0..2^n - 1]] 