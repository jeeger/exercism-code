module Phone (number) where
import Data.Char(isNumber)
import Data.List(isPrefixOf)

number :: String -> Maybe String
number s | length filtered == 10, validNumber filtered = Just filtered
         | ('1':rest) <- filtered, length rest == 10, validNumber rest = Just rest
  where
    validNumber (f:_:_:s:_)@l | f `notElem` "01", s `notElem` "01" = True
    validNumber _ = False
    filtered = filter (`elem` ['0'..'9']) s
number _ = Nothing
