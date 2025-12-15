module Main where

foreign import data Effect :: Type -> Type

data Tuple a b = Tuple a b

foreign import pure :: forall a. a -> Effect a
foreign import bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
foreign import discard :: forall a b. Effect a -> (a -> Effect b) -> Effect b

test :: Effect (Tuple Int Int)
test = do
  x <- pure 1
  y <- pure 2
  pure (Tuple x y)

test' = do
  x <- pure 1
  y <- pure 2
  pure (Tuple x y)
