module Main where

foo :: Int -> Int
foo x @ n = x

bar :: String -> String
bar s @ _ = s

baz :: Int -> Int
baz = (\x @ n -> x)

qux :: Int
qux = let y @ value = 100 in y

foo' x @ n = x

bar' s @ _ = s

baz' = (\x @ n -> x)

qux' = let y @ value = 100 in y
