module Main where

test :: { outer :: { x :: Int } }
test = { outer: { x: 1, y: 2 } }
