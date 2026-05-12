module Main where

foreign import data Effect :: Type -> Type

data Unit = Unit

foreign import pure :: forall a. a -> Effect a
foreign import bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
foreign import discard :: forall a b. Effect a -> (a -> Effect b) -> Effect b

unit :: Unit
unit = Unit

test :: Effect Unit
test = do
  pure 1
  pure unit

test' = do
  pure 1
  pure unit
