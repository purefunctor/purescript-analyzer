module Main where

class Given a where
  consume :: a -> a

testGiven :: forall a. Given a => a -> a
testGiven a = consume a
  where
  b :: Given Int => a
  b = let consumeInt = consume 42 in a

  c :: Int
  c = consume 42
