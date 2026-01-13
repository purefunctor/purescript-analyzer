module Main where

import Data.Profunctor (class Profunctor)

-- Simple function wrapper: a is contravariant, b is covariant
data Fn a b = Fn (a -> b)
derive instance Profunctor Fn

-- Const-like for second param: a is contravariant (consumed), b is phantom
data ConstR r a b = ConstR (a -> r)
derive instance Profunctor (ConstR r)

-- Multiple constructors
data Choice a b = GoLeft (a -> Int) | GoRight b
derive instance Profunctor Choice
