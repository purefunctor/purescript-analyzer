module Main where

import Safe.Coerce (coerce)

testInt :: Int -> Int
testInt = coerce

testString :: String -> String
testString = coerce

testPoly :: forall a. a -> a
testPoly = coerce
