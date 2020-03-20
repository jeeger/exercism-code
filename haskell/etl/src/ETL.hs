{-#LANGUAGE TupleSections#-}
module ETL (transform) where

import Data.Map(Map)
import Data.Char(toLower)
import Data.Foldable(concat)
import qualified Data.Map as M

-- M.Map a [Char] -(mapWithKey (\k v -> [M.Map Char a] -> M.Map Char a
transform :: Integral a => M.Map a [Char] -> M.Map Char a
transform i =  M.unions $ toSingleMaps i where
  -- M.Map a [Char] -> [(a, [Char])] -> [M.Map Char a]
  toSingleMaps i = let lists = M.toList i in
                     concatMap (\(k, v) -> fmap ((flip M.singleton k) . toLower) v) lists
