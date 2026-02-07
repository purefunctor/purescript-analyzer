module Main where

class Empty f where
  empty :: f Int

instance Empty Array where
  empty = []

newtype Vector n a = Vector (Array a)
derive newtype instance Empty (Vector n)

newtype InvalidVector a n = InvalidVector (Array a)
derive newtype instance Empty (InvalidVector Int)
