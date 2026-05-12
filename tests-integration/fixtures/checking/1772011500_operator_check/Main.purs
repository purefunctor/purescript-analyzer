module Main where

infix 5 const as <:

const :: forall a b. a -> b -> a
const a _ = a

infixl 5 add as +

foreign import add :: Int -> Int -> Int
