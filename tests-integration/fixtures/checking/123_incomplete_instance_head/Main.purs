module Main where

class Pair a b where
  pair :: a -> b -> { a :: a, b :: b }

instance Pair Int String where
  pair x y = { a: x, b: y }

instance Pair Int where
  pair x y = { a: x, b: y }
