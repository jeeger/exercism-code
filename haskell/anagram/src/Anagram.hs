module Anagram (anagramsFor) where
import Data.Char(toLower)
import Data.List(sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xxs = filter (\w -> (sort $ lowerFirst xs) == (sort $ lowerFirst w)) xxs
  where lowerFirst (c:cs) = toLower c : cs
        lowerFirst [] = []
