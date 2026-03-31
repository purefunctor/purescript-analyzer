module Main where

type BinaryKind = Type -> Type -> Type

type ConstT :: BinaryKind
type ConstT a b = a
