module Main where

import Safe.Coerce (coerce)

data Maybe :: forall k. k -> Type -> Type
data Maybe n a = Just a | Nothing

newtype MaybeAlias :: forall k. k -> Type -> Type
newtype MaybeAlias n a = MaybeAlias (Maybe n a)

foreign import data Container :: (Type -> Type -> Type) -> Type
type role Container representational

coerceContainer :: Container Maybe -> Container MaybeAlias
coerceContainer = coerce

coerceContainerReverse :: Container MaybeAlias -> Container Maybe
coerceContainerReverse = coerce
