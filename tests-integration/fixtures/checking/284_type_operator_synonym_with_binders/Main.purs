module Main where

data List a = Cons a (List a) | Nil

type Transform f g = forall a. f a -> g a

infixr 4 type Transform as ~>

head :: List ~> List
head (Cons a _) = Cons a Nil
head Nil = Nil
