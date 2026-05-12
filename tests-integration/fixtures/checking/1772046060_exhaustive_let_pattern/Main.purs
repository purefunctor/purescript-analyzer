module Main where

data Maybe a = Just a | Nothing

test =
  let
    Just x = Just 1
  in
    x
