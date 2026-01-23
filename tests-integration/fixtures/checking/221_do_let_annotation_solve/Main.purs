module Main where

foreign import data Effect :: Type -> Type
foreign import data Unit :: Type

foreign import unit :: Unit
foreign import pure :: forall a. a -> Effect a
foreign import bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
foreign import discard :: forall a. Effect a -> Effect Unit -> Effect Unit

thing1 :: Effect String
thing1 = pure "hello"

test :: Effect Unit
test = do
  a <- thing1
  let
    f :: Int
    f = a
  pure unit
