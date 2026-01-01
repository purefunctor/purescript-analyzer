module Main where

type Identity = forall a. a -> a

identity :: Identity
identity a = a

type Const = forall a b. a -> b -> a

const :: Const
const a b = a
