module Pangram (isPangram) where
import Data.Char(chr, isAsciiUpper, toUpper)
import qualified Data.Set as S
import Data.Foldable(foldr')

isPangram :: String -> Bool
isPangram text = S.null $ leftoverChars (map toUpper text)
  where leftoverChars strs = foldr' S.delete (S.fromList ['A'..'Z']) strs
