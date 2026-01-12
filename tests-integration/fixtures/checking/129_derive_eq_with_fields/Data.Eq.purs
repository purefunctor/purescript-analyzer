module Data.Eq where

class Eq a where
  eq :: a -> a -> Boolean

instance Eq Int where
  eq _ _ = true

instance Eq Boolean where
  eq _ _ = true
