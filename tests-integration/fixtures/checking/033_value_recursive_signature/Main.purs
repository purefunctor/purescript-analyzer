module Main where

loop :: forall a. a -> a
loop x = loop x
