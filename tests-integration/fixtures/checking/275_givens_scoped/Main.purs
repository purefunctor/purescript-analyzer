module Main where

class Given a where
  consume :: a -> a

testGiven :: forall a. Given a => a -> a
testGiven a = consume a
  where
  -- b's constraint should be valid in b
  b :: Given Int => a
  b = let consumeInt = consume 42 in a

  -- b's constraint should not leak to c
  c :: Int
  c = consume 42
