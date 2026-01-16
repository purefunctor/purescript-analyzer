module Main where

import Data.Functor (class Functor)

data Predicate a = Predicate (a -> Boolean)
derive instance Functor Predicate

data Reader r a = Reader (r -> a)
derive instance Functor (Reader r)

-- Pass: variance flips twice
data Cont r a = Cont ((a -> r) -> r)
derive instance Functor (Cont r)
