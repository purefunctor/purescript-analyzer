module Main where

foreign import data Effect :: Type -> Type
foreign import data Aff :: Type -> Type

foreign import pure :: forall a. a -> Effect a
foreign import affPure :: forall a. a -> Aff a

foreign import map :: forall a b. (a -> b) -> Effect a -> Effect b
foreign import apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b

test :: Effect { a :: Int, b :: String, c :: Int }
test = ado
  a <- pure 123456
  b <- affPure "life"
  c <- pure 123456
  in { a, b, c }

test' = ado
  a <- pure 123456
  b <- affPure "life"
  c <- pure 123456
  in { a, b, c }
