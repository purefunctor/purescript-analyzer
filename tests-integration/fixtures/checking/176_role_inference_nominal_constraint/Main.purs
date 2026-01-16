module Main where

class Show a where
  show :: a -> String

newtype Shown a = Shown ((Show a => a -> String) -> String)
