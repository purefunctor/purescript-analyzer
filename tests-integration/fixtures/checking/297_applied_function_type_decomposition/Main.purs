module Main where

import Data.Semigroupoid ((<<<))

data Maybe a = Just a | Nothing

-- When `<<<` solves its type variable `p` to `Function`, the result
-- type is `Application(Application(Function, a), b)` rather than
-- the native `Function(a, b)`.  This must be decomposed correctly
-- during function-application checking.

class Foldable f where
  foldMap :: forall a m. (a -> m) -> f a -> m

class Foldable f <= FoldableWithIndex i f where
  foldMapWithIndex :: forall a m. (i -> a -> m) -> f a -> m
  foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b

data NonEmpty f a = NonEmpty a (f a)

instance Foldable f => Foldable (NonEmpty f) where
  foldMap f (NonEmpty a fa) = f a

instance FoldableWithIndex i f => FoldableWithIndex (Maybe i) (NonEmpty f) where
  foldMapWithIndex f (NonEmpty a fa) = f Nothing a
  foldlWithIndex f b (NonEmpty a fa) = foldlWithIndex (f <<< Just) (f Nothing b a) fa
  foldrWithIndex f b (NonEmpty a fa) = f Nothing a (foldrWithIndex (f <<< Just) b fa)
