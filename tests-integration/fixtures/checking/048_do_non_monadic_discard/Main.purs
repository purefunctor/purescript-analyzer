module Main where

bind :: forall a b. a -> (a -> b) -> b
bind x f = f x

discard :: forall a b. a -> (a -> b) -> b
discard x f = f x

test :: Int
test = do
  "ignored"
  42

test' = do
  "ignored"
  42
