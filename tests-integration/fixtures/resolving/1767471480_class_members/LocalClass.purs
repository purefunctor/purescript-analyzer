module LocalClass where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
