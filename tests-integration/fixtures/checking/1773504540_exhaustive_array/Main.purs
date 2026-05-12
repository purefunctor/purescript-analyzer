module Main where

testNonExhaustive :: Array Int -> Int
testNonExhaustive [] = 0
testNonExhaustive [x] = x

testRedundant :: Array Int -> Int
testRedundant [x] = x
testRedundant [y] = y
testRedundant _ = 0

testExhaustive :: Array Int -> Int
testExhaustive [] = 0
testExhaustive [x] = x
testExhaustive _ = 0
