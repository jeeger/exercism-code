module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year | year `divisible` 400 = True
                | year `divisible` 100 = False
                | year `divisible` 4 = True
                | otherwise = False
  where divisible x y = x `mod` y == 0
