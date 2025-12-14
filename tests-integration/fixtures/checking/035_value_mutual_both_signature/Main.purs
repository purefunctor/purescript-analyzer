module Main where

f :: forall a. a -> a
f x = g x

g :: forall a. a -> a
g x = f x
