module Main where

data Box a = Box a

test x = case x of
  Box { a } -> 0
  Box { a, b } -> 0
  Box { a, b, c } -> 0
