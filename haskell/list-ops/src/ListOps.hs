{-# LANGUAGE BangPatterns #-}
module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !z [] = z
foldl' f !z (x:xs) = foldl' f (z `f` x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z (x:xs) = x `f` (foldr f z xs)
foldr _ z [] = z

length :: [a] -> Int
length l = foldr (+) 0 $ map (const 1) l

reverse :: [a] -> [a]
reverse l = helper l [] where
  helper [] l = l
  helper (x:xs) l = helper xs (x:l)


map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map f [] = []

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) = if p x then x : filter p xs else filter p xs
filter p [] = []

(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x: (xs ++ ys)
[] ++ ys = ys

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
