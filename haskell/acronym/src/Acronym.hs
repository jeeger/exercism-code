module Acronym (abbreviate) where
import Data.Char(toUpper, isUpper, isSpace, isPunctuation)
import Data.List(dropWhile, dropWhileEnd)


firstOrCapital:: [Char] -> [Char]
firstOrCapital "" = ""
firstOrCapital s | all isUpper s = [head s]
firstOrCapital (s:ss) = s: filter isUpper ss

myWords:: [Char] -> [[Char]]
myWords "" = []
myWords str = let (word, rest) = break isBreakable str in
                (trim word) : (myWords (dropWhile isBreakable rest))
  where isBreakable x = isSpace x || isPunctuation x
        trim = dropWhileEnd isSpace . dropWhile isSpace

abbreviate :: [Char] -> [Char]
abbreviate xs = (fmap toUpper . firstOrCapital) =<< myWords xs
