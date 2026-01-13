module Data.Show where

class Show a where
  show :: a -> String

instance Show Int where
  show _ = ""
