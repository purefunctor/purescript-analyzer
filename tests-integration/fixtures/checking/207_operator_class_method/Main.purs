module Main where

class Semiring a where
  add :: a -> a -> a

instance Semiring Int where
  add = addImpl

foreign import addImpl :: Int -> Int -> Int

infixl 6 add as +

-- Checking: explicit type annotation
test :: Int
test = 123 + 123

-- Inference: type inferred from operator usage
test' = 123 + 123
