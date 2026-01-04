module Main where

class Show a where
  show :: a -> String

instance Show Boolean where
  show :: forall a. a -> String
  show _ = "bool"
