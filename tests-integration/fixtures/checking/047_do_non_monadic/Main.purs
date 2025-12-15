module Main where

data Tuple a b = Tuple a b

bind :: forall a b. a -> (a -> b) -> b
bind x f = f x

discard :: forall a b. a -> (a -> b) -> b
discard x f = f x

test :: Tuple Int String
test = do
  x <- 1
  y <- "hello"
  Tuple x y

test' = do
  x <- 1
  y <- "hello"
  Tuple x y
