module Main where

data Maybe a = Just a | Nothing

complete = case _, _ of
  Just _, Just _ -> 1
  Just _, Nothing -> 2
  Nothing, Just _ -> 3
  Nothing, Nothing -> 4

incomplete1 = case _, _ of
  Just _, Nothing -> 2
  Nothing, Just _ -> 3
  Nothing, Nothing -> 4

incomplete2 = case _, _ of
  Just _, Just _ -> 1
  Nothing, Just _ -> 3
  Nothing, Nothing -> 4

incomplete3 = case _, _ of
  Just _, Just _ -> 1
  Just _, Nothing -> 2
  Nothing, Nothing -> 4

incomplete4 = case _, _ of
  Just _, Just _ -> 1
  Just _, Nothing -> 2
  Nothing, Just _ -> 3
