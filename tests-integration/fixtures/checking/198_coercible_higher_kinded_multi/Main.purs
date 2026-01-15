module Main where

import Safe.Coerce (coerce)

data Either a b = Left a | Right b

newtype EitherAlias a b = EitherAlias (Either a b)

foreign import data Container :: forall k. k -> Type
type role Container representational

coerceContainer :: Container Either -> Container EitherAlias
coerceContainer = coerce
