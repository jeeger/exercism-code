{-# LANGUAGE ViewPatterns #-}
module Bob (responseFor) where
import Data.Char (isUpper, isAlpha, isSpace)
import Data.List (dropWhileEnd, dropWhile, any, all, isSuffixOf)

trim:: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

responseFor :: String -> String
responseFor (trim -> trimmed)
  | null trimmed = "Fine. Be that way!"
  | asked trimmed, screamed trimmed = "Calm down, I know what I'm doing!"
  | asked trimmed = "Sure."
  | screamed trimmed = "Whoa, chill out!"
  | otherwise = "Whatever."
  where asked = isSuffixOf "?"
        screamed xs = all (\x -> not (isAlpha x) || isUpper x) xs && any isAlpha xs
