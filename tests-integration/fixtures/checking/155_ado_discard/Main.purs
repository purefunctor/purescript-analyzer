module Main where

foreign import data Effect :: Type -> Type

data Unit = Unit

foreign import pure :: forall a. a -> Effect a
foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

unit :: Unit
unit = Unit

test :: Effect Unit
test = ado
  pure 1
  pure unit
  in unit

test' = ado
  pure 1
  pure unit
  in unit
