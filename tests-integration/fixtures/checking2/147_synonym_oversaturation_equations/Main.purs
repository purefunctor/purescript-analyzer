module Main where

type Identity :: forall k. k -> k
type Identity a = a

data Tuple a b = Tuple a b

test1 :: Identity Array Int
test1 = [42]

test2 :: Identity Tuple Int String
test2 = Tuple 42 "hello"

forceSolve = { test1, test2 }
