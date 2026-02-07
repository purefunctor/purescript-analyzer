module Main where

testPartialA :: Char -> Int
testPartialA c = case c of
  'a' -> 1

testPartialB :: Char -> Int
testPartialB c = case c of
  'b' -> 1

testWildcard :: Char -> Int
testWildcard c = case c of
  'a' -> 1
  _ -> 0
