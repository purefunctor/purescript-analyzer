module Main where

data Maybe a = Just a | Nothing

-- Array with nested Maybe constructor - missing Nothing case
testArrayWithMaybe :: Array (Maybe Int) -> Int
testArrayWithMaybe [Just n] = n

-- Array with nested Maybe - multiple elements, missing Nothing cases
testArrayWithMultipleMaybe :: Array (Maybe Int) -> Int
testArrayWithMultipleMaybe [Just n, Just m] = n

-- Exhaustive array with Maybe - wildcard covers remaining cases
testArrayMaybeExhaustive :: Array (Maybe Int) -> Int
testArrayMaybeExhaustive [Just n] = n
testArrayMaybeExhaustive _ = 0

-- Nested arrays with Maybe - complex pattern
testNestedArrayMaybe :: Array (Array (Maybe Int)) -> Int
testNestedArrayMaybe [[Just n]] = n

-- Exhaustive inner case - all Maybe constructors covered plus wildcard for other lengths
testExhaustiveInner :: Array (Maybe Int) -> Int
testExhaustiveInner [Just n] = n
testExhaustiveInner [Nothing] = 0
testExhaustiveInner _ = 0

-- Useless branch with multiple elements - second pattern is redundant
testUselessMultiple :: Array (Maybe Int) -> Int
testUselessMultiple [Just n, Just m] = n
testUselessMultiple [Just a, Just b] = a
testUselessMultiple _ = 0
