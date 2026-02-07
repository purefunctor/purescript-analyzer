module Main where

data Maybe a = Just a | Nothing

test1 = case _ of
  Just _ -> 1

test2 = case _ of
  Nothing -> 2
