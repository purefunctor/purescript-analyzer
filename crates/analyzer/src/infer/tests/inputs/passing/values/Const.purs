module Main where

const a b = a

const1 = let f a b = a in f

const2 = f
  where
  f a b = a
