module Main where

data NonEmpty a = NonEmpty a (Array a)

infixr 5 NonEmpty as :|

test1 (x :| _) = x

test2 (NonEmpty x _) = x

data List a = Cons a (List a) | Nil

infixr 5 Cons as :

test3 = case _ of
  (x : _) -> x
