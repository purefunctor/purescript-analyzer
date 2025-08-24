module Main where

id :: forall a. a -> a
id (a :: a) = (a :: a)

const :: forall a b. a -> b -> a
const a _ =
  let
    z :: Int
    z = a
  in
    z

life :: Int
life = add 21 21
