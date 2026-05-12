module Main where

foreign import data Effect :: Type -> Type

foreign import pure :: forall a. a -> Effect a
foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

test = ado
  x <- pure 42
  y <- pure x
  in { x, y }
