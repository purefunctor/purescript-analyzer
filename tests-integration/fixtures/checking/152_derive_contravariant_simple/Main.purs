module Main where

import Data.Functor.Contravariant (class Contravariant)

data Predicate a = Predicate (a -> Boolean)
derive instance Contravariant Predicate

data Comparison a = Comparison (a -> a -> Boolean)
derive instance Contravariant Comparison

data Op a b = Op (b -> a)
derive instance Contravariant (Op a)
