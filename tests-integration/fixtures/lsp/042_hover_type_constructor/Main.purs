module Main where

foreign import data Effect :: Type -> Type
foreign import data Unit :: Type

foreign import main :: Effect Unit
--                     $
