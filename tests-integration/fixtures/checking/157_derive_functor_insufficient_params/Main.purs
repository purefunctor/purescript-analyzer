module Main where

import Data.Functor (class Functor)

data Pair a b = Pair a b
derive instance Functor (Pair Int String)

data Unit = Unit
derive instance Functor Unit
