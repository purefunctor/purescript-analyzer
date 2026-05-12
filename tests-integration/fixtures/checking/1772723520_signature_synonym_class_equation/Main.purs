module Main where

type ClassHead = Type -> Constraint

class EqLike :: ClassHead
class EqLike a where
  eqLike :: a -> a -> Boolean
