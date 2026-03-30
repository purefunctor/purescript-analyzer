module Main where

class Functor f where
  map :: forall a b. (a -> b) -> (f a -> f b)

instance Functor Array where
  map _ _ = []

testMap = map @Array
