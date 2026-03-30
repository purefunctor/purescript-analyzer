module Main where

test :: { a :: Int } -> Int
test { a, b, c } = a
