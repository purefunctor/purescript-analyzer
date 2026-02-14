module Main where

testPartialZero :: Int -> Int
testPartialZero n = case n of
  0 -> 1

testPartialOne :: Int -> Int
testPartialOne n = case n of
  1 -> 1

testWildcard :: Int -> Int
testWildcard n = case n of
  0 -> 1
  _ -> 0
