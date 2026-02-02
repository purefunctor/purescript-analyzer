module Main where

-- Non-exhaustive case: [] and [x] don't cover all array lengths
testNonExhaustive :: Array Int -> Int
testNonExhaustive [] = 0
testNonExhaustive [x] = x

-- Redundant case: two length-1 patterns, second is redundant
testRedundant :: Array Int -> Int
testRedundant [x] = x
testRedundant [y] = y
testRedundant _ = 0

-- Exhaustive case: wildcard covers all remaining cases
testExhaustive :: Array Int -> Int
testExhaustive [] = 0
testExhaustive [x] = x
testExhaustive _ = 0
