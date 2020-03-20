module Roman (numerals) where

import Data.Maybe (fromJust)

-- Special cases (900, 400, 90, 40, 9, 4) should be generated automatically.
romans = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

-- firstPred is unsafe if there is no element in l satisfying pred.
firstPred  :: (a -> Bool) -> [a] -> a
firstPred pred l = head $ filter pred l
-- Why doesn't the points-free definition firstPred = head . filter work?

numerals :: Integer -> Maybe String
numerals n | n <= 0 = Nothing
numerals n = Just $ unsafeNumerals n ""

unsafeNumerals :: Integer -> String -> String
unsafeNumerals n s | n <= 0 = s
unsafeNumerals n s = let (value, character) = firstPred ((<= n) . fst) romans in
                       unsafeNumerals (n - value) (s ++ character)
  
