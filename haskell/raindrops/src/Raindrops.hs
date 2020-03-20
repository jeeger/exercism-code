module Raindrops (convert) where

plingify n = if n `mod` 3 == 0 then "Pling" else ""
plangify n = if n `mod` 5 == 0 then "Plang" else ""
plongify n = if n `mod` 7 == 0 then "Plong" else ""
convert :: Int -> String
convert n = let plangedResult = (plingify n ++ plangify n ++ plongify n) in
              if null plangedResult then
                show n
              else
                plangedResult
