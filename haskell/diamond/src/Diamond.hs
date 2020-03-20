module Diamond (diamond) where
import Data.List(nub)
import Data.Char(isLetter, isUpper, ord)

mirror l = l ++ tail (reverse l)
-- We could probably replace the sequence mangling here by
-- taking advantage of the mirroring property of the diamond.
-- mirroring both vertically and horizontally.
outerSpaces:: Char -> [Int]
outerSpaces c = let result = take ((chardist 'A' c) + 1) (enumFrom 0) in
  reverse result ++ tail result

innerSpaces:: Char -> [Int]
innerSpaces c = let result = 0: take (chardist 'A' c) (enumFromThen 1 3) in
  result ++ (tail (reverse result)) 

chars:: Char -> [Char]
chars c = let result = ['A'..c] in
  result ++ (tail (reverse result))

diamond :: Char -> Maybe [String]
diamond c | isLetter c, isUpper c =
              Just $ zipWith3 (\a b c -> let outer = replicate a ' '
                                             inner = replicate c ' '
                                         in
                                           case b of
                                             'A' -> outer ++ [b] ++ outer
                                             otherwise -> outer ++ [b] ++ inner ++ [b] ++ outer)
                                             (outerSpaces c) (chars c) (innerSpaces c)
          | otherwise = Nothing


