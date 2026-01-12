module Data.Eq where

class Eq a where
  eq :: a -> a -> Boolean

class Eq1 f where
  eq1 :: forall a. Eq a => f a -> f a -> Boolean

instance Eq Int where
  eq _ _ = true
