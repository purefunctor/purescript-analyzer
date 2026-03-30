module Main where

foreign import data Effect :: Type -> Type

foreign import pure :: forall a. a -> Effect a
foreign import bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
foreign import discard :: forall a b. Effect a -> (a -> Effect b) -> Effect b

test = do
