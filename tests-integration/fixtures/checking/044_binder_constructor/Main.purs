module Main where

data Pair a b = Pair a b

data Maybe a = Nothing | Just a

data List a = Nil | Cons a (List a)

foo :: Pair Int String -> Int
foo (Pair x y) = x

bar :: Maybe Int -> Int
bar (Just n) = n
bar Nothing = 0

baz :: List Int -> Int
baz (Cons h t) = h
baz Nil = 0

qux :: Pair String String -> String
qux (Pair a b) = a

foo' (Pair x y) = x

bar' (Just n) = n
bar' Nothing = 0

baz' (Cons h t) = h
baz' Nil = 0

qux' (Pair a b) = a
