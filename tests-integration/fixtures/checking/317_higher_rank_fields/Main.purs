module Main where

type Test = { identity :: forall a. a -> a }

test1 :: Test -> Int
test1 t = t.identity 42

test2 :: Test -> Int
test2 { identity } = identity 42
