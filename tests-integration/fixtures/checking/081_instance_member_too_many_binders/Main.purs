module Main where

class Show :: Type -> Constraint
class Show a where
  show :: a -> String

instance Show Int where
  show x y = show x
