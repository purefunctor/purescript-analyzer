module Main where

data Maybe a = Just a | Nothing

test1 :: Maybe Int -> Int
test1 (Just _) = 1

test2 :: Maybe Int -> Int
test2 Nothing = 2
