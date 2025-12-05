module Main where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Functor f <= Applicative f where
  pure :: forall a. a -> f a
