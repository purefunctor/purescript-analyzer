module Main where

class MyClass a where
  method :: a -> Int

instance MyClass Int where
  method _ = 42

test :: forall a. MyClass a => a -> Int
test x =
  let
    bar y = method y

    baz :: MyClass Int => Int
    baz = method 42
  in
    bar x
