module Main where

data Maybe a = Just a | Nothing

test a = case a of
  Just 123 -> 123

test' = case _ of
  Just 123 -> 123
