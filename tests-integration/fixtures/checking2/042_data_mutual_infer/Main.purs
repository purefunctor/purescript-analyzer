module Main where

data F a = F (G a)

data G a = G (F a)
