module Main where

testPartialTrue :: Boolean -> Int
testPartialTrue b = case b of
  true -> 1

testPartialFalse :: Boolean -> Int
testPartialFalse b = case b of
  false -> 0

testExhaustive :: Boolean -> Int
testExhaustive b = case b of
  true -> 1
  false -> 0

testNestedPartial :: Boolean -> Boolean -> Int
testNestedPartial x y = case x of
  true -> case y of
    true -> 1
  false -> 0

testWildcardBoolean :: Boolean -> Int
testWildcardBoolean b = case b of
  true -> 1
  _ -> 0
