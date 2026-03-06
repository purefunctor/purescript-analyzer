module Main where

data Tuple a b = Tuple a b

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

class Applicative m <= Discard m where
  discard :: forall a b. m a -> (a -> m b) -> m b

class Applicative m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Bind m <= Monad m

testDo :: forall m. Monad m => m (Tuple Int String)
testDo = do
  x <- pure 1
  y <- pure "hello"
  pure (Tuple x y)

testDo' = do
  x <- pure 1
  y <- pure "hello"
  pure (Tuple x y)

testAdo :: forall f. Applicative f => f (Tuple Int String)
testAdo = ado
  x <- pure 1
  y <- pure "hello"
  in Tuple x y

testAdo' = ado
  x <- pure 1
  y <- pure "hello"
  in Tuple x y

testDoDiscard :: forall m. Monad m => m Int
testDoDiscard = do
  pure "ignored"
  pure 42

testDoDiscard' = do
  pure "ignored"
  pure 42
