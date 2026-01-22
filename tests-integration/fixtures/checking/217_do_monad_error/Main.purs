module Main where

foreign import data Effect :: Type -> Type
foreign import data Aff :: Type -> Type

foreign import pure :: forall a. a -> Effect a
foreign import affPure :: forall a. a -> Aff a

foreign import bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
foreign import discard :: forall a b. Effect a -> (a -> Effect b) -> Effect b

test :: Effect { a :: Int, b :: String, c :: Int }
test = do
  a <- pure 123456
  b <- affPure "life"
  c <- pure 123456
  pure { a, b, c }

