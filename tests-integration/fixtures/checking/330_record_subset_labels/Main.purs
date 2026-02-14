module Main where

data Box a = Box a

test x = case x of
  Box { a, b } -> 0
  Box { a } -> 0
