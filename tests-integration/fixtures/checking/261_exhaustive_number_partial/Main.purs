module Main where

testPartialZero :: Number -> Int
testPartialZero n = case n of
  0.0 -> 1

testPartialOneFive :: Number -> Int
testPartialOneFive n = case n of
  1.5 -> 1

testWildcard :: Number -> Int
testWildcard n = case n of
  0.0 -> 1
  _ -> 0
