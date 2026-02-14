module Main where

class Category :: forall k. (k -> k -> Type) -> Constraint
class Category a where
  identity :: forall t. a t t

instance categoryFn :: Category (->) where
  identity x = x

testFnApp :: (->) Int Int
testFnApp = identity

testAppFn :: Int -> Int
testAppFn = identity
