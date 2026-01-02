module Main where

data Tuple a b = Tuple a b

foreign import pure :: forall a b. a -> b

foreign import map :: forall a b. (a -> b) -> a -> b

foreign import apply :: forall a b. (a -> b) -> a -> b

test :: Tuple Int String
test = ado
  x <- 1
  y <- "hello"
  in Tuple x y

test' = ado
  x <- 1
  y <- "hello"
  in Tuple x y
