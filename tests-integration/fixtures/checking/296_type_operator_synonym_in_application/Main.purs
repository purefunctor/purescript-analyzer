module Main where

type Transform f g = forall a. f a -> g a

infixr 4 type Transform as ~>

data Box a = Box a

unbox :: Box ~> Array
unbox (Box a) = [a]

test :: Array Int
test = unbox (Box 1)
