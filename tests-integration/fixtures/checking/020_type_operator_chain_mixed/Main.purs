module Main where

type Add :: Type -> Type -> Type
type Add a b = a

infixl 5 type Add as +

type Mul :: Type -> Type -> Type
type Mul a b = b

infixl 6 type Mul as *

type Mixed :: Type -> Type -> Type -> Type
type Mixed a b c = a + b * c

type AssocLeft :: Type -> Type -> Type -> Type
type AssocLeft a b c = a + b + c

type AddR :: Type -> Type -> Type
type AddR a b = a

infixr 5 type AddR as -+-

type AssocRight :: Type -> Type -> Type -> Type
type AssocRight a b c = a -+- b -+- c
