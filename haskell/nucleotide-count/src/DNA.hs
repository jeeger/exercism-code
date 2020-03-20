module DNA (nucleotideCounts) where

import qualified Data.Map as Map
import Data.Set (fromList, member, toList)

bases = fromList "ATGC"

countNuc [] map = Right map
countNuc (x:xs) map | member x bases = countNuc xs (Map.adjust (+1) x map)
                    | otherwise = Left "Error"

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts xs = countNuc xs (Map.fromList $ zip (toList bases) (repeat 0))
