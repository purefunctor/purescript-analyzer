module Main where

class Eq a where
  eq :: a -> a -> Boolean

instance Eq Int where
  eq _ _ = false

test = [eq 123 456, eq "123" "456"]
