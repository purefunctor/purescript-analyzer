module Data.Show where

class Show a where
  show :: a -> String

instance Show Int where
  show _ = ""

instance Show a => Show (Array a) where
  show _ = ""
