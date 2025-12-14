module Main where

g x = f x

f :: forall a. a -> a
f x = g x
