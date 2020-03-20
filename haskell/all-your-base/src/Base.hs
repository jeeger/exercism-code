module Base (Error(..), rebase) where

import Control.Monad(foldM)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)


fromBase :: Integral a => a -> [a] -> Either (Error a) a
fromBase base = foldM (\sum d -> if d >= base || d < 0 then Left (InvalidDigit d) else Right (d + sum * base)) 0

toBase :: Integral a => a -> a -> [a]
toBase b n = go n [] where
  go 0 l = l
  go rest l = go (rest `div` b) (rest `mod` b : l)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits | inputBase <= 1 = Left InvalidInputBase
                                        | outputBase <= 1 = Left InvalidOutputBase
                                        | otherwise = toBase outputBase <$> fromBase inputBase inputDigits                                            
