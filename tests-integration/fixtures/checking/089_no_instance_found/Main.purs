module Main where

class Eq a where
  eq :: a -> a -> Boolean

data Foo = Foo

-- No Eq instance for Foo, so this should error
test :: Foo -> Boolean
test x = eq x x
