{-# LANGUAGE ViewPatterns #-}
module RunLength (decode, encode) where

import Data.Foldable(foldl')
import Data.Char(isNumber)

fromDigits:: [Char] -> Integer
fromDigits l = join $ reverse l
  where join [] = 0
        join (i:is) = 10 * join is + (read [i] :: Integer)

myLex:: String -> Maybe (Integer, Char, String)
myLex "" = Nothing
myLex str = case takeWhile isNumber str of
            [] -> Just (1, head str, tail str)
            -- + 1 comes from actual character
            (length -> l) -> Just (fromDigits $ take l str, head $ drop l str, drop (l+1) str)

decode :: String -> String
decode "" = ""
decode str = case myLex str of
               Nothing -> ""
               Just (count, char, rest) -> replicate (fromIntegral count) char ++ decode rest

rlencode:: (Integer, Maybe Char, [Char]) -> Char -> (Integer, Maybe Char, [Char])
rlencode (count, Nothing, str) cletter = (1, Just cletter, "")
rlencode (count, Just lletter, str) cletter | cletter == lletter = (count + 1, Just cletter, str)
rlencode (count, Just lletter, str) cletter = (1, Just cletter, str ++ emit count lletter)

emit:: Integer -> Char -> String
emit 1 c = [c]
emit n c = show n ++ [c]

encode :: String -> String
encode text = case foldl' rlencode (0, Nothing, "") text of
                (_, Nothing, _) -> ""
                (count, Just letter, str) -> str ++ emit count letter

              
