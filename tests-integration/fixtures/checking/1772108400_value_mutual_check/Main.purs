module Main where

f :: forall a. a -> a
f a = g a

g :: forall a. a -> a
g a = f a
