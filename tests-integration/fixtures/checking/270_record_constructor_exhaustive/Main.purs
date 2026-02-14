module Main where

data Maybe a = Just a | Nothing

-- Simple record pattern should be exhaustive
test1 :: { x :: Int, y :: Int } -> Int
test1 { x, y } = x

-- Record with nested Maybe constructor should report missing Nothing
test2 :: { x :: Maybe Int } -> Int
test2 { x: Just n } = n

-- Multiple record patterns - second is redundant since record is exhaustive
test3 :: { x :: Int, y :: Int } -> Int
test3 { x, y } = x
test3 r = 0

-- Record with multiple fields containing constructors - missing Nothing cases
test4 :: { a :: Maybe Int, b :: Maybe String } -> Int
test4 { a: Just n, b: Just _ } = n

-- Nested record patterns should be exhaustive
test5 :: { inner :: { x :: Int } } -> Int
test5 { inner: { x } } = x
