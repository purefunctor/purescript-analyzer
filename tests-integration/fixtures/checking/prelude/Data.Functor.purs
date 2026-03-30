module Data.Functor where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance functorFn :: Functor ((->) r) where
  map f g x = f (g x)
