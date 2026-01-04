module Main where

class TypeEq :: forall k. Type -> Type -> k -> Constraint
class TypeEq a b r | a b -> r where
  identity :: a -> b

instance TypeEq Int Int Int where
  identity :: Int -> Int
  identity x = x

instance TypeEq Boolean Boolean Boolean where
  identity x = x
