module Main where

class Eq a where
  eq :: a -> a -> Boolean

instance Eq a => Eq (Array a) where
  eq _ _ = false
