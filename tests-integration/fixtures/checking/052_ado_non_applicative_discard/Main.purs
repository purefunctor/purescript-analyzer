module Main where

foreign import pure :: forall a b. a -> b

foreign import map :: forall a b. (a -> b) -> a -> b

foreign import apply :: forall a b. (a -> b) -> a -> b

test :: Int
test = ado
  "ignored"
  42
  in 99

test' = ado
  "ignored"
  42
  in 99
