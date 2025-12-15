module Main where

data Tuple a b = Tuple a b

map :: forall a b. (a -> b) -> a -> b
map f x = f x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

test :: Tuple Int String
test = ado
  x <- 1
  y <- "hello"
  in Tuple x y

test' = ado
  x <- 1
  y <- "hello"
  in Tuple x y
