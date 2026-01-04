module Main where

class Show a where
  show :: a -> String

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

data Box a = Box a

instance Functor Box where
  map f (Box x) =
    let
      _ = show x
    in
      Box (f x)
