module Main where

test :: { outer :: { x :: Int } } -> Int
test { outer: { x, y } } = x
