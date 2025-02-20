module Main where

class TypeEq :: forall k. k -> k -> Constraint
class TypeEq (a :: k) (b :: k) where
  proof :: forall p. p a -> p b

class Eq a <= Ord a where
  compare :: a -> a -> Ordering
