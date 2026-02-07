module Main where

testPartialHello :: String -> Int
testPartialHello s = case s of
  "hello" -> 1

testPartialWorld :: String -> Int
testPartialWorld s = case s of
  "world" -> 1

testWildcard :: String -> Int
testWildcard s = case s of
  "hello" -> 1
  _ -> 0
