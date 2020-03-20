module PrimeFactors (primeFactors) where

f n x = (x * x + 1) `mod` n

tortoise n = repeatedly (f n)
hare n = repeatedly (g n) where
  g = (f n) . (f n)

rounds n x = zipWith (,) (iterate (f n) x)

primeFactors :: Integer -> [Integer]
primeFactors n = 
