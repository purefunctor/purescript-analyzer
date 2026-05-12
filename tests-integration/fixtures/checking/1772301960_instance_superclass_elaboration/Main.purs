module Main where

class Eq :: Type -> Constraint
class Eq a where
  eq :: a -> a -> Boolean

class Eq a <= Ord a where
  compare :: a -> a -> Int

test x = if eq x x then compare x x else compare x x
