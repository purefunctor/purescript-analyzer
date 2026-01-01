module Main where

add :: Int -> Int -> Int
add x y = x

infixl 6 add as +

mul :: Int -> Int -> Int
mul x y = x

infixl 7 mul as *

chain :: Int -> Int -> Int -> Int
chain a b c = a + b + c

mixed :: Int -> Int -> Int -> Int
mixed a b c = a + b * c

mixedRev :: Int -> Int -> Int -> Int
mixedRev a b c = a * b + c

paren :: Int -> Int -> Int -> Int
paren a b c = (a + b) * c

chain' a b c = a + b + c

mixed' a b c = a + b * c
