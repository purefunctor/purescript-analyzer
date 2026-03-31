module Main where

class Eq a where
  eq :: a -> a -> Boolean

test :: Int
test = 42
  where
  impl :: forall a. Eq a => a -> Boolean
  impl a = eq a a
