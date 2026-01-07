module Data.Eq where

class Eq a where
  eq :: a -> a -> Boolean
