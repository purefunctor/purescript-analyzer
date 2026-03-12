module Main where

class Eq :: Type -> Constraint
class Eq a where
  eq :: a -> a -> Boolean

class Ord :: Type -> Constraint
class Ord a where
  compare :: a -> a -> Int

test1 x = if eq x x then eq x x else false
test2 x = if eq x x then compare x x else compare x x
