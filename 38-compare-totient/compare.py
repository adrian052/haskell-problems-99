import subprocess
import time
import os
import glob

# Define the Haskell code for each approach as strings
myGCD_cad = '''
import System.CPUTime
myGCD :: Int -> Int -> Int 
myGCD a b 
    | b == 0 = a
    | otherwise = myGCD b (mod a b)

coprimes :: Int -> Int -> Bool
coprimes a b = (myGCD a b) == 1

totient :: Int -> Int
totient n = foldl (\\acc x -> if (coprimes x n) then acc+1 else acc) 0 [1..n-1]

main :: IO ()
main = do
    start <- getCPUTime
    let result = totient 10090
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12) -- Convert to seconds
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Execution time: " ++ show diff ++ " seconds"
'''

prime_factors_cad = '''
import Data.Map (fromListWith, toList)
import System.CPUTime
minPrimeFactor :: Int -> Int
minPrimeFactor n = case [x | x <- [2..isqrt n], n `mod` x == 0] of
                    [] -> n
                    (x:xs) -> x
    where isqrt = floor . sqrt . fromIntegral

factorsUtil :: Int -> [Int] -> [Int]
factorsUtil n curr
    | n == 1 = curr
    | n > 1 = factorsUtil (div n minFactor) (curr ++ [minFactor])
    | otherwise = error "Negative numbers don't have prime factors"
    where minFactor = minPrimeFactor n

factors :: Int -> [(Int,Int)]
factors n
    | n <= 0 = error "Negative numbers don't have prime factors"
    | otherwise = toList (fromListWith (+) [(x, 1) | x <- (factorsUtil n [])])
        
totient' :: Int -> Int
totient' n = foldl (\\acc (p, m) -> acc * (p-1) * p^(m-1)) 1 (factors n)

main :: IO ()
main = do
    start <- getCPUTime
    let result = totient' 10090
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12) -- Convert to seconds
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Execution time: " ++ show diff ++ " seconds"
'''

# Save the Haskell code to temporary files
with open('myGCD.hs', 'w') as file:
    file.write(myGCD_cad)

with open('primeFactors.hs', 'w') as file:
    file.write(prime_factors_cad)

# Compile the Haskell files
subprocess.run(['ghc', '-O2', '-o', 'myGCD_executable', 'myGCD.hs'])
subprocess.run(['ghc', '-O2', '-o', 'primeFactors_executable', 'primeFactors.hs'])

# Execute and measure the execution time for myGCD and coprimes
start_time_myGCD = time.time()
subprocess.run(['./myGCD_executable'])
end_time_myGCD = time.time()

# Execute and measure the execution time for primeFactors
start_time_prime_factors = time.time()
subprocess.run(['./primeFactors_executable'])
end_time_prime_factors = time.time()

# Calculate and display the execution times
execution_time_myGCD = end_time_myGCD - start_time_myGCD
execution_time_prime_factors = end_time_prime_factors - start_time_prime_factors

print("Execution time using myGCD and coprimes:", execution_time_myGCD, "seconds")
print("Execution time using primeFactors:", execution_time_prime_factors, "seconds")

# Remove temporary files and executables
subprocess.run(['rm', 'myGCD.hs', 'myGCD_executable'])
subprocess.run(['rm', 'primeFactors.hs', 'primeFactors_executable'])
for file in  glob.glob('*.hi') + glob.glob('*.o'):
    os.remove(file)