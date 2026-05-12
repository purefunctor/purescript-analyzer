module Main where

class Eq :: Type -> Constraint
class Eq a where
  eq :: a -> a -> Boolean

instance Eq Int where
  eq _ _ = true

instance Eq a => Eq (Array a) where
  eq _ _ = false

test :: Boolean
test = eq [[1]] [[2]]
