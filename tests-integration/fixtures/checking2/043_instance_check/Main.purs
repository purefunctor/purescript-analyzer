module Main where

class Eq :: Type -> Constraint
class Eq a

instance eqArray :: Eq a => Eq (Array a)

class TypeEq :: forall k. k -> k -> Constraint
class TypeEq a b | a -> b, b -> a

instance TypeEq a a
