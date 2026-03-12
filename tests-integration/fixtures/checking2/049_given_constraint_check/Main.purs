module Main where

class Eq a where
  eq :: a -> a -> Boolean

test :: forall a. Eq a => a -> Boolean
test a = eq a a
