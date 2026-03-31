module Main where

class Convert :: Type -> Type -> Constraint
class Convert a b | a -> b where
  convert :: a -> b

instance Convert Int String where
  convert _ = "int"

test = convert 42

class TypeEq :: forall k. k -> k -> Constraint
class TypeEq a b | a -> b, b -> a

instance TypeEq a a
