module Main where

data Box (f :: Type -> Type) (a :: Type) = Box (f a)

type Wrap :: (Type -> Type) -> Type -> Type
type Wrap f = Box f

test :: forall f. Wrap f Int -> Wrap f Int
test x = x
