module Main where

class Show a where
  show :: a -> String

instance Show Int where
  show :: Int -> String
  show _ = "int"
