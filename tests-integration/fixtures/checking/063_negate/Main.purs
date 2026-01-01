module Main where

foreign import negate :: forall a. a -> a

testInt :: Int
testInt = -1

testInt' = -1

testNumber :: Number
testNumber = -1.0

testNumber' = -1.0

testVariable :: Int -> Int
testVariable x = -x

testVariable' x = -x

testNested :: Int
testNested = -(-1)

testNested' = -(-1)
