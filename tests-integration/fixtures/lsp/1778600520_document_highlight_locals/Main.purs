module Main where

infixl 6 add as +

add x y = x

infixr 0 apply as $

apply f x = f x

data Maybe a = Just a | Nothing

pure a = Just a

bind (Just a) f = f a
bind Nothing _ = Nothing

baz = 1

foo = bar + bar + baz
--    &
  where
  bar = 1
--&

foo2 = do
  bar <- Just 1
--&
  pure $ bar + bar
--       &
