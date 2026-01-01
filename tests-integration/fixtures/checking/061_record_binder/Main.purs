module Main where

test1 :: { x :: Int, y :: Int } -> Int
test1 { x, y } = x

test1' { x, y } = x

test2 :: { x :: Int, y :: String } -> { x :: Int, y :: String }
test2 { x, y } = { x, y }

test2' { x, y } = { x, y }

test3 :: { name :: String, age :: Int } -> String
test3 { name: n, age: a } = n

test3' { name: n, age: a } = n

nested :: { inner :: { x :: Int } } -> Int
nested { inner: { x } } = x

nested' { inner: { x } } = x
