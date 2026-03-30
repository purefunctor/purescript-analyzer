module Main where

class Given a where
  consume :: a -> a

testGiven :: forall a. Given a => a -> a
testGiven a = consume a
  where
  b = consume a
  c = consume a
