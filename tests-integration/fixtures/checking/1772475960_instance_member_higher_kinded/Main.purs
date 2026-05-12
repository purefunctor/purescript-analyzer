module Main where

class Functor :: (Type -> Type) -> Constraint
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

newtype Wrap :: forall k. Type -> (k -> Type) -> k -> Type
newtype Wrap e w a = Wrap (w a)

instance Functor w => Functor (Wrap e w) where
  map f (Wrap x) = Wrap (map f x)
