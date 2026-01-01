module Main where

foreign import data Effect :: Type -> Type

foreign import pure :: forall a. a -> Effect a
foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

test = ado
  x <- pure 42
  let y = { x }
  z <- pure "hello"
  in { x, y, z }
