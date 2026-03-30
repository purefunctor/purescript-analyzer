module Main where

class Show :: Type -> Constraint
class Show a where
  show :: a -> String

data Box a = Box a

instance Show (Box a) where
  show :: Box a -> String
  show (Box x) = show x
