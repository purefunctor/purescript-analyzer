module Main where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

data Box a = Box a

instance Functor Box where
  map f (Box x) = Box (f x)
