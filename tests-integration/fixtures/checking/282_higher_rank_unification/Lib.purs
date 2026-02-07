module Lib where

data Maybe a = Nothing | Just a

isJust :: forall a. Maybe a -> Boolean
isJust (Just _) = true
isJust Nothing = false

foreign import data Fn2 :: Type -> Type -> Type -> Type
foreign import data Fn3 :: Type -> Type -> Type -> Type -> Type
foreign import runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c
foreign import runFn3 :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d
