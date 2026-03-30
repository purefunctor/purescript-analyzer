module Main where

data List a = Cons a (List a) | Nil

infixr 5 Cons as :

head :: forall a. List a -> a
head (x : _) = x
