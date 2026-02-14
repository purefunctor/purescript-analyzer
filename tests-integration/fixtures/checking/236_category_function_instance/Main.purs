module Main where

class Semigroupoid :: forall k. (k -> k -> Type) -> Constraint
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

class Category :: forall k. (k -> k -> Type) -> Constraint
class Semigroupoid a <= Category a where
  identity :: forall t. a t t

instance semigroupoidFn :: Semigroupoid (->) where
  compose f g x = f (g x)

instance categoryFn :: Category (->) where
  identity x = x

test :: forall a. a -> a
test = identity

test2 :: Int -> Int
test2 = identity
