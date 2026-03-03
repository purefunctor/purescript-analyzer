module Main where

class Empty f where
  empty :: f Int

instance Empty Array where
  empty = []

newtype Wrapper a = Wrapper (Array a)

derive newtype instance Empty Wrapper
