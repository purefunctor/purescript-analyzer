module Main where

data Maybe a = Just a | Nothing

data Either a b = Left a | Right b

data Tuple a b = Tuple a b

test1 :: Maybe Int -> Int
test1 a = case a of
  Just a -> a
  Nothing -> 0

test1' a = case a of
  Just a -> a
  Nothing -> 0

test2 :: Maybe Int -> Maybe Int -> Int
test2 a b = case a, b of
  Just x, Just y -> x
  Just x, Nothing -> x
  Nothing, Just y -> y
  Nothing, Nothing -> 0

test2' a b = case a, b of
  Just x, Just y -> x
  Just x, Nothing -> x
  Nothing, Just y -> y
  Nothing, Nothing -> 0

test3 :: Either Int String -> Int
test3 e = case e of
  Left x -> x
  Right _ -> 0

test3' e = case e of
  Left x -> x
  Right _ -> 0

test4 :: Tuple (Maybe Int) (Maybe Int) -> Int
test4 t = case t of
  Tuple (Just x) (Just y) -> x
  Tuple (Just x) Nothing -> x
  Tuple Nothing (Just y) -> y
  Tuple Nothing Nothing -> 0

test4' t = case t of
  Tuple (Just x) (Just y) -> x
  Tuple (Just x) Nothing -> x
  Tuple Nothing (Just y) -> y
  Tuple Nothing Nothing -> 0
