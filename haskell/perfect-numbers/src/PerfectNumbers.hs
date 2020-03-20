module PerfectNumbers (classify, Classification(..)) where
import Data.List(nub)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors:: Integral a => a -> [a]
factors 0 = []
factors 1 = []
factors x = let halffactors = filter (\y -> x `mod` y == 0) [1..floor (sqrt (fromIntegral x))]
                otherhalf = map (\y -> x `quot` y) (reverse (tail halffactors)) in
              nub (halffactors ++ otherhalf)

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | sum (factors x) > x = Just Abundant
  | sum (factors x) == x = Just Perfect
  | sum (factors x) < x = Just Deficient
  | otherwise = Nothing
