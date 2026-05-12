module Main where

foreign import data M :: Type -> Type
foreign import data P :: forall k. k -> Type
foreign import data T :: forall k. P k -> Type
