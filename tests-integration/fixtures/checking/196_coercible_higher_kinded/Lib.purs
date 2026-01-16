module Lib where

data Maybe a = Nothing | Just a

newtype MaybeAlias a = MaybeAlias (Maybe a)

foreign import data Container :: forall k. k -> Type
type role Container representational
