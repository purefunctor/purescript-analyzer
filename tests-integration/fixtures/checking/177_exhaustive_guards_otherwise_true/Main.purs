module Main where

import Data.Boolean (otherwise)

foreign import lessThan :: Int -> Int -> Boolean

test :: Int -> Int
test x = case x of
  n
    | lessThan n 0 -> 0
    | otherwise -> n

test' x = case x of
  n
    | lessThan n 0 -> 0
    | otherwise -> n

test2 :: Int -> Int
test2 x = case x of
  n
    | lessThan n 0 -> 0
    | true -> n

test2' x = case x of
  n
    | lessThan n 0 -> 0
    | true -> n
