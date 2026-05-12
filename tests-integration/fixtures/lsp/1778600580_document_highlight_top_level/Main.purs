module Main where

data Int = Int

baz :: Int
baz = Int

foo = bar + bar + baz
--                &
  where
  bar = 1

add x y = x
infixl 6 add as +
