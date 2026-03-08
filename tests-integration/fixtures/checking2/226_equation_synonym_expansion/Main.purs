module Main where

type NaturalTransformation :: (Type -> Type) -> (Type -> Type) -> Type
type NaturalTransformation f g = forall a. f a -> g a

infixr 5 type NaturalTransformation as ~>

foreign import unsafeCoerce :: forall a b. a -> b

test :: forall f. Array ~> f
test a = unsafeCoerce a
