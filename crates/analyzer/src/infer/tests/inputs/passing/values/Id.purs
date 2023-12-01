module Main where

id a = a

id1 = let f x = x in f

id2 = f
  where
  f x = x
