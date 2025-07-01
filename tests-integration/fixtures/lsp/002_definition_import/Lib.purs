module Lib where

life :: Int
life = 42

data Maybe = Just a | Nothing

plus :: Int -> Int -> Int
plus _ _ = 0

infix 5 plus as +
