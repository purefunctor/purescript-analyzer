module Main where

data Maybe a = Just a | Nothing

type NaturalTransformation :: forall k. (k -> Type) -> (k -> Type) -> Type
type NaturalTransformation f g = forall a. f a -> g a

infixr 4 type NaturalTransformation as ~>

type Test1 = NaturalTransformation Array Maybe
type Test2 = Array ~> Maybe
