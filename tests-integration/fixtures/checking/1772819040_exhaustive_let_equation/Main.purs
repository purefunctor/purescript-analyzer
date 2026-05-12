module Main where

data Maybe a = Just a | Nothing

test :: Int
test =
  let
    f :: Maybe Int -> Int
    f (Just _) = 1
  in f Nothing

test2 :: Int
test2 =
  let
    g :: Maybe Int -> Int
    g Nothing = 1
  in g (Just 42)
