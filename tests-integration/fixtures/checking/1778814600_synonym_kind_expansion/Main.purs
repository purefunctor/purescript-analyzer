module Main where

type TypeToType = Type -> Type

type ApplyAliasKind :: TypeToType -> Type
type ApplyAliasKind f = f Int
