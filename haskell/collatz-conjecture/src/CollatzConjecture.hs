module CollatzConjecture (collatz) where

collam:: Integral a => a -> [a]
collam = iterate (\x -> if even x then x `quot` 2 else 3 * x + 1)

collatz :: Integer -> Maybe Integer
collatz x | x <= 0 = Nothing
collatz x = Just . toInteger . length . takeWhile (1 /=) $ collam x
