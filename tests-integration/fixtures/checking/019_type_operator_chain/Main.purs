module Main where

type Add :: Type -> Type -> Type
type Add a b = a

infixl 5 type Add as +

type Chain :: Type -> Type -> Type -> Type
type Chain a b c = a + b + c

type ChainParen :: Type -> Type -> Type -> Type
type ChainParen a b c = (a + b) + c
