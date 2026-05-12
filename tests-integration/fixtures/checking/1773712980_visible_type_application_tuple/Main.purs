module Main where

data Tuple a b = Tuple a b

testBoth = Tuple @Int @String
testLeft = Tuple @Int @_
testRight = Tuple @_ @String
