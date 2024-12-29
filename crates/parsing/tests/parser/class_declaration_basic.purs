module ClassDeclaration where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Monad m where
  bind :: forall a b. m a -> (a -> m b) -> m b
  return :: forall a. a -> m a
