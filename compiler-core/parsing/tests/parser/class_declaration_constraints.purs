module ClassDeclarationConstraints where

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

class (Functor f, Applicative f) <= Monad f where
  return :: forall a. a -> f a
