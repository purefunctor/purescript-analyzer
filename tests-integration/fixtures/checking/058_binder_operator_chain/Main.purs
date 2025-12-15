module Main where

data List a = Nil | Cons a (List a)

infixr 5 Cons as :

matchCons :: List Int -> Int
matchCons (x : y : Nil) = x
matchCons _ = 0

matchSingle :: List Int -> Int
matchSingle (x : Nil) = x
matchSingle _ = 0

matchCons' (x : y : Nil) = x
matchCons' _ = 0
