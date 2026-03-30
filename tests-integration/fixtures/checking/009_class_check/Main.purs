module Main where

class Eq :: Type -> Constraint
class Eq a where
  eq :: a -> a -> Boolean
