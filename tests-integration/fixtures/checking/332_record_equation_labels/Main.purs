module Main where

data Box a = Box a

test (Box { a }) = 0
test (Box { a, b }) = 0
