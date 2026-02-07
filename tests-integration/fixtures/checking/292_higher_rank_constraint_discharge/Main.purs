module Main where

import Lib (class Apply, apply, class Functor, map, class Foldable, foldr, Fn2, Fn3, runFn2, runFn3)

-- When a class method like `apply` (Apply f => ...) is passed as an
-- argument to a function expecting a higher-rank type without constraints
-- (forall a' b'. m (a' -> b') -> m a' -> m b'), the constraint must be
-- peeled as a wanted rather than leaking into unification. Reproduces
-- the traverse1Impl pattern from Data.Array.NonEmpty.Internal.
foreign import impl3
  :: forall m a b
   . Fn3
       (forall a' b'. (m (a' -> b') -> m a' -> m b'))
       (forall a' b'. (a' -> b') -> m a' -> m b')
       (a -> m b)
       (m b)

test :: forall m a b. Apply m => Functor m => (a -> m b) -> m b
test f = runFn3 impl3 apply map f

-- Similar pattern with Foldable: fromFoldable = runFn2 impl2 foldr
foreign import impl2
  :: forall f a
   . Fn2 (forall b. (a -> b -> b) -> b -> f a -> b) (f a) (Int)

test2 :: forall f a. Foldable f => f a -> Int
test2 = runFn2 impl2 foldr
