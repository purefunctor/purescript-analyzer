module Main where

data Tree a = Tree a (Forest a)
data Forest a = Nil | Cons (Tree a) (Forest a)
