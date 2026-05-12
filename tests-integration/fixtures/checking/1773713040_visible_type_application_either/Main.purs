module Main where

data Either a b = Left a | Right b

testBothLeft = Left @Int @String
testBothRight = Right @Int @String

testLeftLeft = Left @Int @_
testLeftRight = Left @_ @String

testRightLeft = Right @Int @_
testRightRight = Right @_ @String
