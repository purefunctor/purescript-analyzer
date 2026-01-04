module Main where

class Show a where
  show :: a -> String

class Functor f where
  map :: forall a b. Show a => (a -> b) -> f a -> f b

data Box a = Box a

data Maybe a = Nothing | Just a

instance Functor Box where
  map f (Box x) =
    let
      _ = show x
    in
      Box (f x)

instance Functor Maybe where
  map f Nothing = Nothing
  map f (Just x) = Just (f x)
