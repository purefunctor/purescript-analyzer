module Main where

class HasKind :: forall k. k -> Constraint
class HasKind a where
  reflectKind :: forall (p :: k -> Type). p a -> String
