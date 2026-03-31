module Main where

class Eq a where
  eq :: a -> a -> Boolean

infix 5 eq as ==
