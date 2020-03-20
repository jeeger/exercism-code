{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthState
  ,  ForthError (..)
  , evalText
  , toList
  , empty
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Char (isSpace, isNumber)
import Data.Map ((!?))
import qualified Data.Map as M

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord Text
  deriving (Show, Eq)


data ForthState a =  ForthState { stack :: [a]
                                , defWords :: M.Map Text (ForthState a -> Either ForthError (ForthState a))
                                }

type ForthInterpreterState a = Either ForthError (ForthState a)


instance Show a => Show (ForthState a) where
  show (ForthState s w) = show s ++ " " ++ show (M.keys w)

builtins :: Integral a => M.Map Text (ForthState a -> ForthInterpreterState a)
builtins = M.fromList
           [ ("+", binop (+))
           , ("-", binop (-))
           , ("/", fdiv)
           , ("*", binop (*))
           , (T.toCaseFold "dup", fdup)
           , (T.toCaseFold "drop", fdrop)
           , (T.toCaseFold "swap", fswap)
           , (T.toCaseFold "over", fover)
           ]

empty :: Integral a => ForthState a
empty = ForthState { stack = [], defWords = builtins }

isNum :: Text -> Bool
isNum w = T.takeWhile isNumber w == w

flex :: Text -> (Text, Text)
flex t = T.strip <$> T.span (not . isSpace) t

getDefinition :: Text -> (Text, Text, Text)
getDefinition t = let (def, rest) = T.span (/= ';') t
                      (word, code) = flex def
                      restcode = T.drop 2 rest in
                    (T.toCaseFold word, code, restcode)

evalText :: (Num a, Read a) => Text -> ForthState a -> ForthInterpreterState a
evalText text = go (flex text) where
  go ("", _) fs = Right fs
  go (":", rest) fs = let (name, def, prog) = getDefinition rest in
                        if isNum name then
                          Left InvalidWord
                        else
                          go (flex prog) (fs {defWords = M.insert name (evalText def) (defWords fs) })
  go (word, rest) fs = if isNum word then
                         push fs (read $ T.unpack word) >>=
                         go (flex rest)
                       else
                         case defWords fs !? T.toCaseFold word of
                           Just f -> f fs >>=
                                     go (flex rest)
                           Nothing -> Left (UnknownWord word)

                                         
toList :: Num a => ForthState a -> [a]
toList = reverse . stack

push :: Num a => ForthState a -> a -> ForthInterpreterState a
push ForthState { stack=stack, defWords=w } i = Right ForthState { stack = i:stack, defWords=w }

pop :: Num a => ForthState a -> Either ForthError (a, ForthState a)
pop fs@ForthState { stack = (s:ss) } = Right (s,  fs {stack = ss})
pop ForthState { stack = []} = Left StackUnderflow

peek :: Num a => ForthState a -> Either ForthError a
peek ForthState {stack = stack@(s:ss)} = Right s
peek ForthState {stack = []} = Left StackUnderflow

binop :: Num a => (a -> a -> a) -> ForthState a -> ForthInterpreterState a
binop fn fs = do
  (arg1, s1) <- pop fs
  (arg2, s2) <- pop s1
  push s2 (arg2 `fn` arg1)

fdiv :: Integral a => ForthState a -> ForthInterpreterState a
fdiv fs = do
  (arg1, fs) <- pop fs
  (arg2, fs) <- pop fs
  if arg1 == 0 then
    Left DivisionByZero
    else
    push fs (arg2 `div` arg1)
    

fdup :: Num a => ForthState a -> ForthInterpreterState a
fdup fs = do
  arg1 <- peek fs
  push fs arg1

fdrop :: Num a => ForthState a -> ForthInterpreterState a
fdrop fs = do
  (_, s2) <- pop fs
  return s2

fswap :: Num a => ForthState a -> ForthInterpreterState a
fswap fs = do
  (arg2, fs) <- pop fs
  (arg1, fs) <- pop fs
  fs <- push fs arg2
  push fs arg1


fover :: Num a => ForthState a -> ForthInterpreterState a
fover fs = do
  (arg1, fs) <- pop fs
  arg2 <- peek fs
  fs <- push fs arg1
  push fs arg2
  
  
