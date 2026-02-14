module Main where

data Maybe a = Just a | Nothing

test x = case x of
  Nothing -> 0
  Just { a } -> 0
  Just { a, b } -> 0
