module Lib where

class Apply (f :: Type -> Type) where
  apply :: forall a b. f (a -> b) -> f a -> f b

class Functor (f :: Type -> Type) where
  map :: forall a b. (a -> b) -> f a -> f b

class Foldable (f :: Type -> Type) where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b

data Fn2 a b c = Fn2

foreign import runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c

data Fn3 a b c d = Fn3

foreign import runFn3 :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d
