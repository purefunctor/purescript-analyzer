module Main where

foreign import negate :: forall a. a -> a

testRecord :: { a :: Int, b :: Int }
testRecord =
  let
    negate a = { a, b: a }
  in
    -0

testRecord' =
  let
    negate a = { a, b: a }
  in
    -0

testArray :: Array Int
testArray =
  let
    negate a = [a, a]
  in
    -0

testArray' =
  let
    negate a = [a, a]
  in
    -0

testFunction :: Int -> Int
testFunction =
  let
    negate a b = a
  in
    -0

testFunction' =
  let
    negate a b = a
  in
    -0
