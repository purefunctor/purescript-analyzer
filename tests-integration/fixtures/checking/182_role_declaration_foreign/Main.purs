module Main where

foreign import data Effect :: Type -> Type

type role Effect representational
