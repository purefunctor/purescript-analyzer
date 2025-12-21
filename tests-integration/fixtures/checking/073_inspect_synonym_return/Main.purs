module Main where

type ReturnsInt a = a -> Int

test1 :: forall a. a -> ReturnsInt a
test1 a b = 42

test2 :: forall a. ReturnsInt a -> ReturnsInt a
test2 f = f

type ReturnsResult a b = a -> b -> Int

test3 :: forall a b. a -> b -> ReturnsResult a b
test3 a b c d = 42
