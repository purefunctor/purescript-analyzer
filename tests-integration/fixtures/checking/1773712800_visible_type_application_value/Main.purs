module Main where

identity :: forall @a. a -> a
identity a = a

const :: forall a @b. a -> b -> a
const a _ = a

testIdentity = identity @Int
testConst = const @String
