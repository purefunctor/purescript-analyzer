module Main where

testGuarded :: Boolean -> Int
testGuarded true | false = 1
testGuarded false = 0

testGuardedBoth :: Boolean -> Int
testGuardedBoth true | true = 1
testGuardedBoth false | true = 0
