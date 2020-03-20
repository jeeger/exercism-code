module SumOfMultiples (sumOfMultiples) where


sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter divisibleByAny [1..(pred limit)]
  where divisibleByAny num = any (\x -> num `mod` x == 0) factors
