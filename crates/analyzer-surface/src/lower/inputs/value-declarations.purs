module Main where

const :: forall a b. a -> b -> a
const a b = a

basicGuards
  | 1909 = 'm'
  | 99.0 = "a"

bindGuard
  | a <- b = 0
  | c <- d = 1

mixedGuard a
  | b <- a
  , b = { a, b, c }
  where
  c = 0
