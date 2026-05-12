module Main (foo, Bar(..), class C, baz) where

-- !

data Bar
  = Baz
  | Qux Int

foo :: Int
foo = 1

baz :: Int -> Int
baz x = x

class C a where
  c :: a -> a
