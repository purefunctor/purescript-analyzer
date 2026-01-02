module Main where

foreign import data Effect :: Type -> Type

foreign import pure :: forall a. a -> Effect a
foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

test1 = ado

test2 = ado
  in 123
