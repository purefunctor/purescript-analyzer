module Main where

import Safe.Coerce (coerce)

data Maybe a = Nothing | Just a
data List a = Nil | Cons a (List a)

foreign import data Container :: (Type -> Type) -> Type
type role Container representational

coerceContainerDifferent :: Container Maybe -> Container List
coerceContainerDifferent = coerce
