module Main where

data Foo = Foo

class Eq a where
  eq :: a -> a -> Boolean

-- Let binding without signature has residual constraint
-- Since there's no Eq Foo instance, this should error
test =
  let f x = eq x x
  in f Foo
