module Main where

data Unit = Unit

class Example a where
  example :: a

test = case _ of
  Unit -> example
