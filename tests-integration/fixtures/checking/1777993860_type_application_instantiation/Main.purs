module Main where

const :: forall @a b. a -> b -> a
const a _ = a

data Either a b = Left a | Right b

forceSolve =
  { const: const @Int
  , left: Left @Int
  , right: Right @Int
  }
