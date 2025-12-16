module Main where

test1 :: Array Int -> { x :: Int, y :: Int }
test1 [x, y] = { x, y }

test1' [x, y] = { x, y }

test2 :: forall a. Array a -> Array a
test2 [x, y, z] = [z, y, x]

test2' [x, y, z] = [z, y, x]

test3 :: Array Int -> Int
test3 [] = 0
test3 [x] = x
test3 [x, y] = x

test3' [] = 0
test3' [x] = x
test3' [x, y] = x

nested :: Array (Array Int) -> Int
nested [[x]] = x
nested [[x, y], [z]] = x

nested' [[x]] = x
nested' [[x, y], [z]] = x
