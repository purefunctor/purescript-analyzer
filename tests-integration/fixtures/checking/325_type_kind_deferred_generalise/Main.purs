module Main where

foreign import data Component :: forall k. k -> Type

data ForceTypeType :: (Type -> Type) -> Type
data ForceTypeType f = ForceTypeType

data Known :: forall k. Component k -> Type -> Type
data Known a b = Known (ForceTypeType k)

infix 4 type Known as +++
