module Main where

class Const :: (Type -> Type) -> Constraint
class Const f where
  const :: forall a. f a -> forall b. f b -> f a
