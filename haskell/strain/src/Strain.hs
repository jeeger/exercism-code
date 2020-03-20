module Strain (keep, discard) where

import Control.Monad(guard)

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = keep (not . p) xs

-- Desugared list comprehension.
keep :: (a -> Bool) -> [a] -> [a]
keep p xs = do
  x <- xs
  guard (p x)
  return x

