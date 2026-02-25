module Main where

data Maybe a = Just a | Nothing

test :: Maybe Int -> Int
test (Just _) = 1
