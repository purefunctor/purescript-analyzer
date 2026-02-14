module Main where

foreign import data Component :: forall k. k -> Type

data ForceTypeType :: (Type -> Type) -> Type
data ForceTypeType f = ForceTypeType

knownInEquation :: forall m. Component m -> Component m -> Int
knownInEquation _ _ =
  let
    forced :: ForceTypeType m
    forced = ForceTypeType
  in
    42

infix 4 knownInEquation as +++
