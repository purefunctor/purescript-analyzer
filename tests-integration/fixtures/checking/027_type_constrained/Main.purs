module Main where

class Show a where
  show :: a -> String

type Showable = Show Int => Int
