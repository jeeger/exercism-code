{-# LANGUAGE ViewPatterns #-}
module Isogram (isIsogram) where
import qualified Data.Set as S
import Data.Char(toLower,isLetter)

isIsogram :: String -> Bool
isIsogram s = isogram_helper s S.empty where
  isogram_helper "" _ = True
  -- View pattern applies toLower to head and stores result in c.
  isogram_helper ((toLower -> c):cs) s | isLetter c = if c `S.member` s then
                              False
                            else
                              isogram_helper cs (S.insert c s)
  isogram_helper (c:cs) s = isogram_helper cs s
