module Main where

type Add :: Type -> Type -> Type
type Add a b = a

infixl 5 type Add as +

type Bad = Int + "hello"
