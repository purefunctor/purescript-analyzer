module Main where

identity :: forall a. a -> a
identity a = a

test = identity @Int
