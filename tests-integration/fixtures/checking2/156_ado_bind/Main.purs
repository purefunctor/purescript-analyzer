module Main where

foreign import data Effect :: Type -> Type

data Tuple a b = Tuple a b

foreign import pure :: forall a. a -> Effect a
foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

test :: Effect (Tuple Int Int)
test = ado
  x <- pure 1
  y <- pure 2
  in Tuple x y

test' = ado
  x <- pure 1
  y <- pure 2
  in Tuple x y
