module Main where

data Maybe a = Just a | Nothing

fromJust :: forall a. Partial => Maybe a -> a
fromJust (Just x) = x

test :: forall a. Int
test =
  let
    silent :: Partial => Maybe a -> a
    silent (Just x) = x

    loud :: Maybe a -> Int
    loud Nothing = 42
  in
    42
