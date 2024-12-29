module TypeSynonym where

type Year :: Type
type Year = Int

type Identity :: Type -> Type
type Identity a = a
