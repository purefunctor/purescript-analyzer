module Main where

foreign import data Effect :: Type -> Type
foreign import data Unit :: Type

foreign import unit :: Unit
foreign import pure :: forall a. a -> Effect a
foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

thing1 :: Effect String
thing1 = pure "hello"

test :: Effect Unit
test = ado
  a <- thing1
  let
    f :: Int
    f = a
  in unit
