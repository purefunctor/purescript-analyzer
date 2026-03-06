module Main where

class C a where
  c :: a -> Int

instance cBool :: C Boolean where
  c true = 1
