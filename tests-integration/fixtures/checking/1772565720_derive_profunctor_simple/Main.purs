module Main where

import Data.Profunctor (class Profunctor)

data Fn a b = Fn (a -> b)
derive instance Profunctor Fn

data ConstR r a b = ConstR (a -> r)
derive instance Profunctor (ConstR r)

data Choice a b = GoLeft (a -> Int) | GoRight b
derive instance Profunctor Choice
