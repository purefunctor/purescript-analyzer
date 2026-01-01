module Main where

data Tuple a b = Tuple a b

foreign import map :: forall f a b. (a -> b) -> f a -> f b
foreign import apply :: forall f a b. f (a -> b) -> f a -> f b

foreign import pure :: forall f a. a -> f a

test :: forall f. f (Tuple Int String)
test = ado
  x <- pure 1
  y <- pure "hello"
  in Tuple x y

test' = ado
  x <- pure 1
  y <- pure "hello"
  in Tuple x y
