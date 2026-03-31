module Main where

class Category :: forall k. (k -> k -> Type) -> Constraint
class Category a where
  identity :: forall t. a t t

instance categoryFn :: Category (->) where
  identity x = x

test :: (->) Int Int
test = identity

test' :: Int -> Int
test' = identity
