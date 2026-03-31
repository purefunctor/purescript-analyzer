module Main where

class Category f where
  identity :: forall a. f a a

instance Category Function where
  identity :: forall a. a -> a
  identity a = a
