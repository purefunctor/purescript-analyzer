module Main where

data Tuple a b = Tuple a b

data Maybe a = Just a | Nothing

complete = case _ of
  Tuple (Just _) (Just _) -> 1
  Tuple (Just _) Nothing -> 2
  Tuple Nothing (Just _) -> 3
  Tuple Nothing Nothing -> 4

incomplete1 = case _ of
  Tuple (Just _) Nothing -> 2
  Tuple Nothing (Just _) -> 3
  Tuple Nothing Nothing -> 4

incomplete2 = case _ of
  Tuple (Just _) (Just _) -> 1
  Tuple Nothing (Just _) -> 3
  Tuple Nothing Nothing -> 4

incomplete3 = case _ of
  Tuple (Just _) (Just _) -> 1
  Tuple (Just _) Nothing -> 2
  Tuple Nothing Nothing -> 4

incomplete4 = case _ of
  Tuple (Just _) (Just _) -> 1
  Tuple (Just _) Nothing -> 2
  Tuple Nothing (Just _) -> 3
