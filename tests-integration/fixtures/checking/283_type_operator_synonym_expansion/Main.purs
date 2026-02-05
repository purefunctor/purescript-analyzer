module Main where

data Maybe a = Just a | Nothing

type NaturalTransformation f g = forall a. f a -> g a

infixr 4 type NaturalTransformation as ~>

test :: Maybe ~> Maybe
test (Just a) = Just a
test Nothing = Nothing

test' :: NaturalTransformation Maybe Maybe
test' (Just a) = Just a
test' Nothing = Nothing
