module ClassLibrary where

class Show a where
  show :: a -> String

class Eq a where
  eq :: a -> a -> Boolean
  notEq :: a -> a -> Boolean

class Ord a where
  compare :: a -> a -> Int
