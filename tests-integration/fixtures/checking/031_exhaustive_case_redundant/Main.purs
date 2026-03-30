module Main where

data Maybe a = Just a | Nothing

test = case _ of
  Just _ -> 1
  Just _ -> 2
  Nothing -> 3
