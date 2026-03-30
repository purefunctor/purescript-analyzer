module Main where

data F :: forall k. k -> Type
data F a = F (G a)

data G :: forall k. k -> Type
data G a = G (F a)
