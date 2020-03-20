module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

import Data.Foldable(foldr')

data LinkedList a = Elem { getValue::a
                         , getNext:: LinkedList a} | Nil deriving (Eq, Show)

datum :: LinkedList a -> a
datum = getValue

fromList :: [a] -> LinkedList a
fromList = foldr' new Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Elem x linkedList

next :: LinkedList a -> LinkedList a
next e@(Elem _ _) = getNext e
next Nil = Nil

nil :: LinkedList a
nil = Nil

-- Not the nicest implementation.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Nil = Nil
reverseLinkedList l = sreverse l Nil where
  sreverse Nil e = e
  sreverse (Elem v1 n1) e = sreverse n1 (Elem v1 e)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Elem value next) = value : toList next
