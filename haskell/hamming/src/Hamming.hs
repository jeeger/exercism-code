module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just (sum $ zipWith (\x y -> if x /= y then 1 else 0) xs ys)
