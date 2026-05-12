module Main where

variable :: Array Int -> Int
variable = case _ of
  [ x ] -> x
  xs | length <- arrayLength xs -> length

wildcard :: Array Int -> Int
wildcard = case _ of
  [ x ] -> x
  xs | _ <- arrayLength xs -> 0

refutable :: Array Int -> Int
refutable = case _ of
  [ x ] -> x
  xs | [ y ] <- xs -> y

foreign import arrayLength :: forall a. Array a -> Int
