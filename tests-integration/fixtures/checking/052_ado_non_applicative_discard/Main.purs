module Main where

map :: forall a b. (a -> b) -> a -> b
map f x = f x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

test :: Int
test = ado
  "ignored"
  42
  in 99

test' = ado
  "ignored"
  42
  in 99
