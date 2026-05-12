module Main where

data Tuple a b = Tuple a b

foreign import bind :: forall m a b. m a -> (a -> m b) -> m b
foreign import discard :: forall m a b. m a -> (a -> m b) -> m b

foreign import pure :: forall m a. a -> m a

test :: forall m. m (Tuple Int String)
test = do
  x <- pure 1
  y <- pure "hello"
  pure (Tuple x y)

test' = do
  x <- pure 1
  y <- pure "hello"
  pure (Tuple x y)
