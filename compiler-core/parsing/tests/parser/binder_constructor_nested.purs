module BinderConstructorNested where

test a = case a of
  Tuple Nothing Nothing -> 0
  Tuple (Just a) Nothing -> 1
  Tuple Nothing (Just b) -> 2
  Tuple (Just a) (Just b) -> 3
