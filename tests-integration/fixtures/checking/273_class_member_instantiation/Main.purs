module Main where

data Unit = Unit

class Monoid a where
  mempty :: a

class Semigroup a where
  append :: a -> a -> a

foreign import apply :: forall a b. (a -> b) -> a -> b
foreign import pure :: forall m a. a -> m a
foreign import lift2 :: forall m a b c. (a -> b -> c) -> m a -> m b -> m c

test1 :: forall a. Monoid a => Unit -> a
test1 _ = mempty

test2 = \_ -> mempty

test3 = apply (\x -> x) mempty

test4 = pure mempty

test5 = lift2 append
