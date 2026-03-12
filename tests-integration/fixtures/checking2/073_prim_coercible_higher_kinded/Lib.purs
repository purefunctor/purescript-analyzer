module Lib where

data Maybe a = Nothing | Just a

newtype MaybeAlias a = MaybeAlias (Maybe a)

foreign import data Container :: (Type -> Type) -> Type
type role Container representational
