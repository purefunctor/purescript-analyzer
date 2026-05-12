module Main where

test :: { a :: Int, b :: Int } -> Int
test { a } = a
