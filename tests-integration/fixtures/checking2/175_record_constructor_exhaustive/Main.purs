module Main where

data Maybe a = Just a | Nothing

test1 :: { x :: Int, y :: Int } -> Int
test1 { x, y } = x

test2 :: { x :: Maybe Int } -> Int
test2 { x: Just n } = n

test3 :: { x :: Int, y :: Int } -> Int
test3 { x, y } = x
test3 r = 0

test4 :: { a :: Maybe Int, b :: Maybe String } -> Int
test4 { a: Just n, b: Just _ } = n

test5 :: { inner :: { x :: Int } } -> Int
test5 { inner: { x } } = x
